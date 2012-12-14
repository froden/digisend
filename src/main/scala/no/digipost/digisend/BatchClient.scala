package no.digipost.digisend

import xml.NodeSeq
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.io.ByteArrayOutputStream
import fr.janalyse.ssh.SSH

trait BatchClient {
  def zip(xml: NodeSeq, files: Map[String, Array[Byte]]) = {
    def entry(zipStream: ZipOutputStream, name: String, data: Array[Byte]) {
      zipStream.putNextEntry(new ZipEntry(name))
      zipStream.write(data)
      zipStream.closeEntry()
    }

    val dataOut = new ByteArrayOutputStream()
    val zip = new ZipOutputStream(dataOut)
    entry(zip, "masseutsendelse.xml", xml.toString().getBytes("utf-8"))
    files.foreach { case (filename, file) => entry(zip, filename, file) }
    zip.close()
    dataOut.toByteArray
  }

  def sftpUpload(senderId: String, filename: String, fileData: Array[Byte], passphrase:String = null, host: String = "sftp.digipost.no") {
    SSH.ftp(host = host, username = "qa_" + senderId, passphrase = passphrase) {
      ftp => ftp.putBytes(fileData, "/masseutsendelse/" + filename)
    }
  }
}
