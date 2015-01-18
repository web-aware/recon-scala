package net.coeffect.recon

import org.scalatest._

class ReconSpec
  extends FlatSpec
  with ReconBehaviors
  with ReconStringBehaviors {

  override def suiteName = "Recon specification"

  it should behave like SerializesRecon(Recon)

  it should behave like InterpolatesReconLiterals(Recon)
  it should behave like SubstitutesReconVariables(Recon)
}
