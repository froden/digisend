package no.digipost.digisend

import org.apache.commons.io.IOUtils

object Util {
  def fileAsStream(filename: String) = getClass.getClassLoader.getResourceAsStream(filename)

  def fileAsBytes(filename: String) = IOUtils.toByteArray(fileAsStream(filename))
}
