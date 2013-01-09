package no.digipost.digisend

import no.digipost.api.client.representations._
import scala.xml.{Node, NodeSeq}
import java.lang.Integer
import collection.JavaConverters._
import XmlTypes._
import no.digipost.api.client.DigipostClient
import Util._

trait RestClient {
  def sendRestApi(client: DigipostClient)(messages: Seq[(String, Message)]) {
    messages.foreach {
      case (filename, msg) => try {
        client.sendMessage(msg, fileAsStream(filename))
      } catch {
        case ex: Throwable => println(ex)
      }
    }
  }

  def xmlToMessages(xml: NodeSeq) = {
    val forsendelser = xml \\ "forsendelse"
    val recipients = forsendelser.map { f =>
      val brevId = (f \\ "brev").text

      val mottaker = (f \\ "mottaker") match {
        case NavnOgAdresse(FulltNavnFornavnForst(navn), Adresse(adresse, postnummer, poststed)) =>
          new RecipientIdentification(new NameAndAddress(navn, adresse, null, postnummer, poststed))
        case Digipostadresse(d) => new RecipientIdentification(new DigipostAddress(d))
      }
      (brevId, mottaker)
    }

    recipients.map {
      case (brevId, mottaker) => {
        val brevXml: Node = (xml \\ "dokument").find(n => (n \\ "id").text == brevId).get
        val Brev(id, fil, emne, _) = brevXml
        val smsvarsling = brevXml match {
          case Smsvarsling(tidspunkter, etterTimer) =>
            new SmsNotification(tidspunkter.map(new ListedTime(_)).asJava, etterTimer.map(new Integer(_)).asJava)
          case Smsvarsling18(true) => new SmsNotification(0)
          case _ => new SmsNotification()
        }
        (fil, message((math.random * 100000).toString, emne, mottaker, smsvarsling))
      }
    }
  }

  def client(senderId: Int, certificate: String, certificatePassword: String = "Qwer1234!", logging: Boolean = false,
             host: String = "https://qa.api.digipost.no") = {
    val certificateStream = fileAsStream(certificate)
    val client = new DigipostClient(host, senderId, certificateStream, certificatePassword)
    if (logging) client.addFilter(new TestLoggingFilter())
    client
  }

  def printRecipient(name: String, address: String, postalCode: String, city: String) = new PrintRecipient(name, new NorwegianAddress(address, postalCode, city))

  def message(messageId: String, subject: String, recipient: RecipientIdentification, smsNotification: SmsNotification = null,
              authLevel: AuthenticationLevel = AuthenticationLevel.PASSWORD, sensitivityLevel: SensitivityLevel = SensitivityLevel.NORMAL) =
    new Message(messageId, subject, recipient, smsNotification, authLevel, sensitivityLevel)
}
