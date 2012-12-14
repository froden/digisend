package no.digipost.digisend

import no.digipost.api.client.DigipostClient
import no.digipost.api.client.representations._
import java.math.BigDecimal
import org.joda.time.{DateTime, LocalDate}
import no.digipost.api.client.representations.PrintDetails.PostType._
import Api._
import collection.JavaConverters._

object Api {
  def client(senderId: Int, certificate: String, certificatePassword: String = "Qwer1234", logging: Boolean = false,
             host: String = "https://qa.api.digipost.no") = {
    val certificateStream = fileAsStream(certificate)
    val client = new DigipostClient(host, senderId, certificateStream, certificatePassword)
    if (logging) client.addFilter(new TestLoggingFilter())
    client
  }

  def fileAsStream(filename: String) = getClass.getClassLoader.getResourceAsStream(filename)

  def printRecipient(name: String, address: String, postalCode: String, city: String) = new PrintRecipient(name, new NorwegianAddress(address, postalCode, city))

  def message(messageId: String, subject: String, recipient: RecipientIdentification, smsNotification: SmsNotification = null,
              authLevel: AuthenticationLevel = AuthenticationLevel.PASSWORD, sensitivityLevel: SensitivityLevel = SensitivityLevel.NORMAL) =
    new Message(messageId, subject, recipient, smsNotification, authLevel, sensitivityLevel)
}

object InvoiceTest extends App {
  val c = client(179074, "certificate.p12")
  val recipient = new PersonalIdentificationNumber("12345678909")
  val recipientAddress = printRecipient("Frozzat Nerbrattski", "testvegen 2", "1109", "Oslo")
  val returnAddress = printRecipient("Digipost", "Postgirobygget", "0011", "Oslo")
  val printDetails = new PrintDetails(recipientAddress, returnAddress, B)
    val message = new Invoice("invoiceski5", "Rematest", new RecipientIdentification(recipient, printDetails), new SmsNotification(),
      AuthenticationLevel.PASSWORD,
      SensitivityLevel.NORMAL,
      "33333333333", new BigDecimal("900.09"), "12345678909", new LocalDate())
  c.sendMessageToDigipostOrDeliverToPrint(message, ContentType.PDF, getClass.getResourceAsStream("/gyldig-for-print.pdf"))
}

object NameAndAddressMatch extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse", new RecipientIdentification(
    new NameAndAddress("Frode Nerbråten", "Hovsvingen 28", null, "2069", "Jessheim")))
  client(179709, "certificate_hackaton_prod.p12", logging = true, host = "https://api.digipost.no").sendMessage(msg, fileAsStream("brev.pdf"))
}

object NameAndAddressNotMatchWithFallbackToPrint extends App {
  val recipientAddress = printRecipient("Janniche Haugen", "Testgate 1", "1109", "Oslo")
  val returnAddress = printRecipient("Digipost", "Postgirobygget", "0011", "Oslo")
  val printDetails = new PrintDetails(recipientAddress, returnAddress, B)
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse", new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Feilgate 16", null, "0573", "Oslo"), printDetails))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("gyldig-for-print.pdf"))
}

object DirectToPrint extends App {
  val recipientAddress = printRecipient("Janniche Haugen", "Testgate 1", "1109", "Oslo")
  val returnAddress = printRecipient("Digipost", "Postgirobygget", "0011", "Oslo")
  val printDetails = new PrintDetails(recipientAddress, returnAddress, B)
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse", new RecipientIdentification(printDetails))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("gyldig-for-print.pdf"))
}

object NameAndAddressMatchWithHighSensitivityLevelAndTwoFactor extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse med høy sensitivitet og tofaktor", new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Testgate 16", null, "0573", "Oslo")), authLevel = AuthenticationLevel.TWO_FACTOR, sensitivityLevel =
    SensitivityLevel.SENSITIVE)
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}

object NameAndAddressMatchWithHighSensitivityLevelAndTwoFactorAndPreEncrypt extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse med høy sensitivitet, tofaktor og prekryptert",
    new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Testgate 16", null, "0573", "Oslo")), authLevel = AuthenticationLevel.TWO_FACTOR, sensitivityLevel =
    SensitivityLevel.SENSITIVE)
  msg.setPreEncrypt()
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}

object NameAndAddressWithSpecialCharsMatch extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST navn og adresse spesialtegn", new RecipientIdentification(
    new NameAndAddress("  Janniche HAugen ", "Testgate,  \"16", null, "0573", "Oslo")))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}


object BirthdateAndPhoneNumberMatch extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST fødselsdato og telefonnummer", new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Feilgate 16", null, "0573", "Oslo", new LocalDate(1985, 11, 18), "93883196", null)))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}

object BirthdateAndEmailAddressMatch extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST fødselsdato og epostadresse", new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Feilgate 16", null, "0573", "Oslo", new LocalDate(1985, 11, 18), null, "janniche.haugen@BEKK.no")))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}

object BirthdateAndEmailAddressMatchWithSmsNotification extends App {
  val msg = message("msg" + System.currentTimeMillis(), "REST fødselsdato og epostadresse", new RecipientIdentification(
    new NameAndAddress("Janniche Haugen", "Feilgate 16", null, "0573", "Oslo", new LocalDate(1985, 11, 18), null, "janniche.haugen@BEKK.no")),
    new SmsNotification(List(new ListedTime(new DateTime(2012, 12, 23, 10, 0))).asJava, List[java.lang.Integer](2, 3, 5).asJava))
  client(179074, "certificate.p12", logging = true).sendMessage(msg, fileAsStream("rema.pdf"))
}
