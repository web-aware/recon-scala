package recon

import basis.collections._
import basis.text._

class ReconException(message: String, val input: Iterator[Int]) extends RuntimeException(message) {
  def this(message: String) = this(message, Iterator.empty)

  override def toString: String = (String.Builder~"ReconException"~'('~>message~", "~>input~')').state
}
