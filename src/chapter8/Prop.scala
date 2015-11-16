package chapter8

import chapter8.Prop.{SuccessCount, FailedCase}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop { def check: Either[FailedCase, SuccessCount] }