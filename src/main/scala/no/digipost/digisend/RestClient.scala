package no.digipost.digisend

import no.digipost.api.client.representations._
import no.digipost.digisend.Api._
import scala.xml.{Node, NodeSeq}
import java.lang.Integer
import XmlTypes._

trait RestClient {
  def sendRestApi(senderId:String, messages: Seq[(String, Message)]) {
    val c = client(senderId.toInt, "certificate.p12", logging = true, host = "https://qa.api.digipost.no")
    messages.foreach {
      case (filename, msg) => try {
        c.sendMessage(msg, fileAsStream(filename))
      } catch {
        case ex: Throwable => println(ex)
      }
    }
  }

  def xmlToMessages(xml: NodeSeq) = {
    val forsendelser = xml \\ "forsendelse"
    val recipients = forsendelser.map { f =>
      val brevId = (f \\ "brev").text

      val recipient = (f \\ "mottaker") match {
        case NavnOgAdresse(FulltNavnFornavnForst(navn), Adresse(adresse, postnummer, poststed)) =>
          new RecipientIdentification(new NameAndAddress(navn, adresse, null, postnummer, poststed))
        case Digipostadresse(d) => new RecipientIdentification(new DigipostAddress(d))
      }
      (brevId, recipient)
    }

    recipients.map {
      case (brevId, r) => {
        val brevXml: Node = (xml \\ "dokument").find(n => (n \\ "id").text == brevId).get
        val Dokument(fil, emne) = brevXml
        val SmsVarsling(tidspunkter, etterTimer) = brevXml
        (fil, message((math.random * 100000).toString, emne, r,
          new SmsNotification(tidspunkter.map(new ListedTime(_)), etterTimer.map(new Integer(_)))))
      }
    }
  }
}
