package workflow

/**
  * Created by hinoue on 2017/06/15.
  */

import rescala._
import scala.util.{Failure, Success, Try}
import scalaz.Free.Trampoline
import scalaz.Trampoline._
import scala.languageFeature.implicitConversions

class TransactionError extends Exception
object RestartTE extends TransactionError
object AbortTE extends TransactionError

trait Context
object Restart extends Context
object Abort extends Context
object Continue extends Context

trait Workflow[A]{ self =>
  def execute[B](ctx:Signal[Context])(g:A => Trampoline[Try[B]]): Trampoline[Try[B]]

  def flatMap[C](f: A => Workflow[C]): Workflow[C] =
    new Workflow[C]{
      def execute[D](ctx:Signal[Context])(g:C => Trampoline[Try[D]]): Trampoline[Try[D]] =
        self.execute(ctx)(a => f(a).execute(ctx)(g))
    }

  def map[C](f: A => C): Workflow[C] = flatMap(a => Workflows.point(f(a)))

  def withFilter(p:A => Boolean):Workflow[A] = this

//  private[workflow] def runrun[B](e:Signal[Context])(next:A => Trampoline[Try[B]]):Trampoline[Try[B]] = this.execute(e)(next)

  private[workflow] def runnable(ctx:Signal[Context]):Trampoline[Try[A]] = this.execute[A](ctx)((x:A) => done(Success(x)))

  private[workflow] def run(ctx:Signal[Context]):Try[A] = this.execute[A](ctx)((x:A) => done(Success(x))).run
}

object Workflows extends scalaz.Monad[Workflow] {

  val empty_proc: () => Unit = () => ()
  val empty_comp: Any => Unit = (_: Any) => ()

  def unit[A](a: => A):Workflow[A] = atom(a)(empty_comp)

  def point[A](a: => A):Workflow[A] = unit(a)

  def bind[A, B](a: Workflow[A])(f: A => Workflow[B]): Workflow[B] =
    a.flatMap(f)

  def run[A](t:Workflow[A],createSC: () => Signal[Context] = () => Var(Continue)):Try[A] =
    t.run(createSC()) match {
      case Failure(RestartTE) => run(t,createSC)
      case e => e
    }

  private def atom[A](proc: => A)(comp: A => Unit = empty_comp): Workflow[A] =
    new Workflow[A] {
      def execute[B](ctx: Signal[Context])(g: A => Trampoline[Try[B]]): Trampoline[Try[B]] =
        Try(proc) match {
          case Failure(ee) => done(Failure(ee))
          case Success(a) =>
            suspend {
              g(a).flatMap {
                case Failure(ee) =>
                  ee match {
                    case _: TransactionError => comp(a)
                  };done(Failure(ee))
                case Success(x) => done(Success(x))
              }
            }
        }
    }

  def catom[A](proc: => A)(comp: A => Unit = empty_comp): Workflow[A] =
    new Workflow[Unit] {
      def execute[B](ctx: Signal[Context])(g: Unit => Trampoline[Try[B]]): Trampoline[Try[B]] =
        ctx.now match {
          case Abort => done(Failure(AbortTE))
          case Restart => done(Failure(RestartTE))
          case Continue => g()
        }
    }.flatMap((_:Unit) => atom(proc)(comp))
  
  def sub[A](t:Workflow[A]): Workflow[A] =
    new Workflow[A]{
      def execute[B](ctx: Signal[Context])(g:A => Trampoline[Try[B]]):Trampoline[Try[B]] = {
        suspend {
          t.runnable(ctx) flatMap {
            case Success(x) => g(x)
            case Failure(e) => done(Failure(e))
          }
        }
      }
    }

  implicit def toWorkflow[A](proc: => A): Workflow[A] =
    catom(proc)(empty_comp)

  implicit def toWorkflowOps[A](proc: => A): WorkflowOps[A] =
    new WorkflowOps[A](catom(proc)(empty_comp))

//  implicit def toWorkflowOps[A](t: Workflow[A]): WorkflowOps[A] =
//    new WorkflowOps[A](t)

  class WorkflowOps[A](t: Workflow[A]) {
    // Add compensation to the STransaction t
    def %%(comp: A => Unit): Workflow[A] =
      for {
        a <- t
        _ <- atom(a)(comp)
      } yield a
  }
}