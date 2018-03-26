package com.github.grender.wv_tt

import org.scalatest.FlatSpec

class SharesNamesTest extends FlatSpec {
  "SharesName" should "correct create from string" in {

    assert(SharesName.fromLetter("A")==SharesName.A)
    assert(SharesName.fromLetter("B")==SharesName.B)
    assert(SharesName.fromLetter("C")==SharesName.C)
    assert(SharesName.fromLetter("D")==SharesName.D)

    val exception = intercept[Exception] {
      SharesName.fromLetter("xxx")
    }
    assert(exception.getMessage.startsWith("Invalid sharesName string value"))
  }
}