package spinal.lib.generator

import spinal.core._
import spinal.core.internals.classNameOf

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}


class Unset
object Unset extends  Unset{

}

object Dependable{
  def apply[T](d : Dependable*)(body : => T) = {
    val h = Handle[T]
    Generator.stack.head.add {
      val p = new Generator()
      p.dependencies ++= d
      p.add task {h.load(body)}
      p.products += h
      p
    }
    h
  }
}


trait Dependable{
  @dontName val products = ArrayBuffer[Handle[_]]()

  def isDone : Boolean
  def produce[T](body : => T) : Handle[T] = Dependable(this)(body)
  def produce[T](h : Handle[T])(body : => T)  : Handle[T] = Dependable(h, this)(body)
  def produceIo[T <: Data](body : => T) : Handle[T] = {
    val h = Handle[T]
    products += h
    Generator.stack.head.add {
      val p = new Generator()
      p.dependencies += this
      p.add task {h.load{
        val subIo = body
        val topIo = cloneOf(subIo).setPartialName(h, "", true)
        topIo.copyDirectionOf(subIo)
        topIo <> subIo
        topIo
      }
      1}
      p
    }
    h
  }
}

case class Lock() extends Dependable{
  var retains = 0
  def retain() : Unit = retains += 1
  def release() : Unit = retains -= 1
  override def isDone: Boolean = retains == 0
}

object Handle{
  def apply[T](value : =>  T) : Handle[T] = {
    val h = Handle[T]
    h.lazyDefaultGen = () => value
    h
  }
  def apply[T]() = new Handle[T]
  implicit def handleToHandle[T, T2 <: T](h : Handle[T2]) : Handle[T] = h.asInstanceOf[ Handle[T]]
  implicit def keyImplicit[T](key : Handle[T]): T = key.get
  implicit def keyImplicit[T](key : Seq[Handle[T]]): Seq[T] = key.map(_.get)
  implicit def initImplicit[T](value : T) : Handle[T] = Handle(value)
  implicit def initImplicit[T](value : Unset) : Handle[T] = Handle[T]
  implicit def handleDataPimped[T <: Data](key : Handle[T]): DataPimper[T] = new DataPimper(key.get)

  implicit def miaouImplicitHandle[T](value : Handle[T]) = new {
    def yolo[T2](body : (T) => T2) = value.produce(body(value))
  }
}

trait HandleCoreSubscriber{
  def changeCore(core : HandleCore) : Unit
  def lazyDefault (): Any
  def lazyDefaultAvailable : Boolean
}

class HandleCore{
  private var loaded = false
  private var value : Any = null

  val subscribers = mutable.HashSet[HandleCoreSubscriber]()

  def get : Any = {
    if(!loaded){
      subscribers.count(_.lazyDefaultAvailable) match {
        case 0 =>
        case 1 => load(subscribers.find(_.lazyDefaultAvailable).get.lazyDefault())
        case _ => SpinalError("Multiple handle default values")
      }
    }
    value
  }
  def load(value : Any): Any = {
    this.value = value
    loaded = true
    value
  }

  def merge(that : HandleCore): Unit ={
    (this.loaded, that.loaded) match {
      case (false, _) => this.subscribers.foreach(_.changeCore(that))
      case (true, false) => that.subscribers.foreach(_.changeCore(this))
      case _ => ???
    }
  }

  def isLoaded = loaded || subscribers.exists(_.lazyDefaultAvailable)
}

class Handle[T] extends Nameable with Dependable with HandleCoreSubscriber{
  val generator = Generator.stack.headOption.getOrElse(null)
  var core = new HandleCore
  core.subscribers += this

  override def changeCore(core: HandleCore): Unit = {
    this.core = core
    core.subscribers += this
  }

  def merge[T2 <: T](that : Handle[T2]): Unit = this.core.merge(that.core)

  def apply : T = get.asInstanceOf[T]
  def get: T = core.get.asInstanceOf[T]
  def load[T2 <: T](value : T2): T2 = core.load(value.asInstanceOf[Any]).asInstanceOf[T2]
//  def load[T2 <: T](value : Handle[T2]): T2 = core.load(if(value == null) null else value.get.asInstanceOf[Any]).asInstanceOf[T2]
  def loadAny(value : Any): Unit = core.load(value.asInstanceOf[T])

  def isLoaded = core.isLoaded

  override def isDone: Boolean = isLoaded

  var lazyDefaultGen : () => Any = null
  override def lazyDefault() : T = lazyDefaultGen().asInstanceOf[T]
  override def lazyDefaultAvailable: Boolean = lazyDefaultGen != null

  override def toString: String = (if(generator != null) generator.toString + "/" else "") + super.toString
}




object Generator{
  def stack = GlobalData.get.userDatabase.getOrElseUpdate(Generator, new Stack[Generator]).asInstanceOf[Stack[Generator]]
}



case class Product[T](src :() => T, handle : Handle[T])

class Generator(@dontName constructionCd : Handle[ClockDomain] = null) extends Nameable with Dependable with DelayedInit with TagContainer with OverridedEqualsHashCode{
  if(Generator.stack.nonEmpty && Generator.stack.head != null){
    Generator.stack.head.generators += this
  }

  Generator.stack.push(this)
  var elaborated = false
  @dontName implicit var c : GeneratorCompiler = null
//  @dontName implicit val p : Plugin = this
  @dontName val dependencies = ArrayBuffer[Dependable]()
  @dontName val tasks = ArrayBuffer[Task[_]]()
  @dontName val generators = ArrayBuffer[Generator]()

  case class Task[T](gen : () => T, handle : Handle[T]){
    def build() = handle.load(gen())
  }


  def createDependency[T] = {
    val handle = Handle[T]
    dependencies += handle
    handle
  }
  def product[T] : Handle[T] = {
    val handle = Handle[T]()
    products += handle
    handle
  }

  var implicitCd : Handle[ClockDomain] = null
  if(constructionCd != null) onClockDomain(constructionCd)

  var useClockDomain = true
  def noClockDomain(): Unit = useClockDomain = false
  def onClockDomain(clockDomain : Handle[ClockDomain]): this.type ={
    implicitCd = clockDomain
    dependencies += clockDomain
    this
  }

  def apply[T](body : => T): T = {
    Generator.stack.push(this)
    val b = body
    Generator.stack.pop()
    b
  }
//  {
//    val stack = Composable.stack
//    if(stack.nonEmpty) stack.head.generators += this
//  }

  //User API
//  implicit def lambdaToGenerator[T](lambda : => T) = new Task(() => lambda)
  def add = new {
    def task[T](gen : => T) : Handle[T] = {
      val handle = Handle[T]
      products += handle
      tasks += new Task(() => gen, handle)
      handle
    }
  }
  def add[T <: Generator](generator : => T) : T = {
//    generators += generator
    apply(generator)
  }

  def generateIt(): Unit ={
    if(implicitCd != null) implicitCd.push()

    apply {
      for (task <- tasks) {
        task.build()
        task.handle.get match {
          case n: Nameable => {
            n.setCompositeName(this, true)
          }
          case _ =>
        }
      }
    }
    if(implicitCd != null) implicitCd.pop()
    elaborated = true
  }

  override def isDone: Boolean = elaborated


  override def delayedInit(body: => Unit) = {
    body
    if ((body _).getClass.getDeclaringClass == this.getClass) {
      Generator.stack.pop()
    }
  }


  def toComponent(): GeneratorComponent[this.type] = new GeneratorComponent(this)


  def foreachGeneratorRec(body : Generator => Unit): Unit ={
    generators.foreach{ g =>
      body(g)
      g.foreachGeneratorRec(body)
    }
  }

  def export[T](h : Handle[T]) = {
    h.produce(this.tags += new Export(h.getName, h.get))
    h
  }
  def dts[T <: Nameable](node : Handle[T])(value : => String) = add task {
    node.produce(this.tags += new Dts(node, value))
    node
  }
}


class GeneratorCompiler {
//  Composable.stack.push(this)
  val rootGenerators = ArrayBuffer[Generator]()
  val database = mutable.LinkedHashMap[Any, Any]()
  def add(that : Generator) = rootGenerators += that
  def build(): Unit = {
    implicit val c = this
    println(s"Build start")
    val generatorsAll = mutable.LinkedHashSet[Generator]()
    def scanGenerators(generator : Generator, clockDomain : Handle[ClockDomain]): Unit ={
      if(!generatorsAll.contains(generator)){
        if(generator.useClockDomain && generator.implicitCd == null && clockDomain != null)
          generator.onClockDomain(clockDomain)
        generatorsAll += generator
        generator.reflectNames()
        generator.c = this
        val splitName = classNameOf(generator).splitAt(1)
        if(generator.isUnnamed) generator.setWeakName(splitName._1.toLowerCase + splitName._2)
      }
      for(child <- generator.generators) scanGenerators(child, generator.implicitCd)
    }

    def scanRoot() = for(generator <- rootGenerators) scanGenerators(generator, null)
    scanRoot()

    var step = 0
    while(generatorsAll.exists(!_.elaborated)){
      println(s"Step $step")
      var progressed = false
      for(generator <- generatorsAll if !generator.elaborated && generator.dependencies.forall(_.isDone)){
        if(generator.isNamed && generator.getName != "generator") println(s"Build " + generator.getName)
        generator.generateIt()
        progressed = true
      }
      if(!progressed){
        val unelaborateds = generatorsAll.filter(!_.elaborated)
        val missingDepedancies = unelaborateds.flatMap(_.dependencies).toSet.filter(!_.isDone)
        val missingHandle = missingDepedancies.filter(_.isInstanceOf[Handle[_]]).map(_.asInstanceOf[Handle[Any]])
        val producatable = unelaborateds.flatMap(_.products).map(_.core).toSet
        SpinalError(
          s"Composable hang, remaings generators are :\n" +
          s"${unelaborateds.map(p => s"- ${p} depend on ${p.dependencies.filter(d => !d.isDone).mkString(", ")}\n").mkString}" +
          s"\nDependable not completed :\n" +
          s"${missingDepedancies.map(d => "- " + d + "\n").mkString}" +
          s"\nHandles without potential sources :\n" +
          s"${missingHandle.filter(e => !producatable.contains(e.core)).map(d => "- " + d + "\n").mkString}"
        )
      }
      step += 1
      scanRoot()
    }
//    Composable.stack.pop()
  }
}

object GeneratorComponent{
  implicit def toGenerator[T <: Generator](g : GeneratorComponent[T]) = g.generator
}

class GeneratorComponent[T <: Generator](val generator : T) extends Component{
  val c = new GeneratorCompiler()
  c.rootGenerators += generator
  generator.setName("")
  c.build()
  generator.setName("")
  this.setDefinitionName(classNameOf(generator))
}
