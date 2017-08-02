package test

/**
  * Created by hinoue on 2017/07/21.
  */

import javax.xml.transform.sax.SAXTransformerFactory

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.MustMatchers._
import workflow._
import Workflows._
import rescala._


class WorkflowLawsTest extends FunSuite {
  val ST = Workflows
  val u = Workflows.point(0)
  val f = (x:Int) => catom{if(x == 0) true else false}()
  val g = (b:Boolean) => catom{if(b) 1 else 0}()

  test("right identity"){
    def rightIdentity[A](x: Workflow[A]): Boolean ={
      ST.run(x.flatMap(a => point(a))) == ST.run(x)
    }
    assert(rightIdentity(u))
  }

  test("left identity"){
    def leftIdentity[A,B](x: A, f: A => Workflow[B]): Boolean ={
      ST.run(point(x).flatMap(f)) == ST.run(f(x))
    }

    def liAbort[A,B](x: A, f: A => Workflow[B]): Boolean ={
      ST.run(point(x).flatMap(f),Var(Abort)) == ST.run(f(x),Var(Abort))
    }
    assert(leftIdentity(3,f))
    assert(leftIdentity(false,g))

    // if point is catom, it will fail
    assert(liAbort(1,(x:Int) => point(x)))
  }

  test("associativity"){
    def associativity[A,B,C](x: Workflow[A], f: A => Workflow[B], g: B => Workflow[C]): Boolean = {
      ST.run(x.flatMap(f).flatMap(g)) ==
        ST.run(x.flatMap(a => f(a).flatMap(g)))
    }

    assert(associativity(u,f,g))
  }

  test("large transaction"){
    var i = 0
    val wf = for{
      _ <- () %% (_ => println("a"))
      x <- replicateM(10000,{i+=1} %% (ii => println(ii)))
    } yield ()

    assert(ST.run(wf).isSuccess)
  }

  test("compensation"){
    import workflow._
    import Workflows._
    import scala.util.{Try,Success,Failure}

    val time = Var[Context](Continue)

    var i = 0

    val t = for{
      _ <- (i += 1) %% (_ => i -= 2)
      _ <- time() = Abort
      _ <- (i += 2)
    } yield ()

    assert(ST.run(t,time).isFailure)
    assert(i == -1)

  }
}
