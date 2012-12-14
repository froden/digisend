package no.digipost.digisend

import com.sun.jersey.api.client.filter.LoggingFilter
import no.digipost.api.client.DigipostClient
import no.digipost.api.client.representations._
import java.math.BigDecimal
import org.joda.time.LocalDate


object Test extends App {
  val stream = getClass.getResourceAsStream("/certificate.p12")
//  val senderId = 265005
  val prodSender = 179709
  val client = new DigipostClient("https://api.digipost.no", prodSender, stream, "Qwer1234")
  //client.addFilter(new LoggingFilter())
  val recipient = new PersonalIdentificationNumber("23068046351")
  val message = new Invoice("smokey8", "Test i qa", recipient, new SmsNotification(), AuthenticationLevel.PASSWORD, SensitivityLevel.NORMAL, "33333333333", new BigDecimal("900.09"), "12345678909", new LocalDate())
//  val message = new Message("smokey7", "Test i qa", recipient, new SmsNotification(), AuthenticationLevel.PASSWORD, SensitivityLevel.NORMAL)
  client.sendMessage(message, getClass.getResourceAsStream("/brev.pdf"))

  println("ferdig")
}