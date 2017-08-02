/**
  * Created by hinoue on 2017/08/02.
  */
package object workflow {
  implicit val M:scalaz.Monad[Workflow] = Workflows
}
