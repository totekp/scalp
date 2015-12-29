package org.scalp

object ScalpUtil {

  def withClose[AA, BB, CC](init: AA)(opFun: AA => BB, closeFun: AA => CC) = {
    try {
      opFun(init)
    } finally {
      closeFun(init)
    }
  }
}
