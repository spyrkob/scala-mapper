package com.bspyrkos.scalamapper.api

/**
 * Created by spyrkob on 26/09/2014.
 */
trait PojoConverter {
  implicit def conv[T](get: => (() => T)):Convertion[T] = new Convertion[T](get)

  implicit def listConv[T](get: ()=> Seq[T]):ListConvertion[T] = new ListConvertion[T](get)

  def map[T](get: => () => T):Convertion[T] = new Convertion[T](get)
  def map[T](get: => () => Seq[T]):ListConvertion[T] = new ListConvertion[T](get)

  class ListConvertion[T](get: => ()=>Seq[T]) {
    var _def:Option[T] = None
    def default(d: T) = {
      _def = Option(d)
      this
    }

    def into (set:(Seq[T])=>Unit) = {
      val opt = optional(get)
      if (opt.isEmpty){
        _def.foreach(d=>set(List(d)))
      }else {
        set(opt)
      }
    }

    def intoSingle (set:(T)=>Unit) = {
      val opt = optional(get)
      if (opt.isEmpty){
        _def.foreach(set)
      }else {
        opt.foreach(set)
      }
    }

    def as [S] (converter:(T)=>S) = {
      new ListConvertion[S](() => get().map(converter))
    }

    def flatMap[S](converter:(T)=>Seq[S]) = {
      new ListConvertion[S](() => get().flatMap(converter))
    }

    private def optional[T](check: =>()=>Seq[T]): Seq[T] = {
      try {
        check().filter(_ != null)
      } catch {
        case e:NullPointerException => Nil
        case e:Exception => throw e
      }
    }

    def filter(filterFunc: (T) => Boolean) = {
      new ListConvertion[T](()=>get().filter(filterFunc))
    }
  }

  class Convertion[T](get: => ()=>T) {
    var _def:Option[T] = None
    var cond:(T)=>Boolean = (_)=>true
    def default(d: T) = {
      _def = Option(d)
      this
    }

    def into (set:(T)=>Unit) = {
      val opt = optional(get).filter(cond).orElse(_def)
      opt.foreach(set(_))
    }

    def as [S] (converter:(T)=>S) = {
      new Convertion[S](() => converter(get()))
    }

    def filter(filterFunc: (T) => Boolean) = {
      cond = filterFunc
      this
    }

    private def optional[T](check: =>()=>T): Option[T] = {
      try {
        Option(check())
      } catch {
        case e:NullPointerException => None
        case e:Exception => throw e
      }
    }
  }
}
