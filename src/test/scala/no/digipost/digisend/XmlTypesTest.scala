package no.digipost.digisend

import org.scalatest.FunSuite
import no.digipost.digisend.XmlTypes._
import org.joda.time.DateTime

class XmlTypesTest extends FunSuite {

  test("Digipostadresse skal kunne lage og lese xml") {
    Digipostadresse("frode.tester#1234") match {
      case Digipostadresse("frode.tester#1234") => ()
    }
  }

  test("NavnOgAdresse med FulltNavnFornavnFors skal kunne lage og lese xml") {
    NavnOgAdresse(FulltNavnFornavnForst("Frode Nerbråten"), Adresse("Engate 34", "1234", "Oslo"), "kundeid") match {
      case NavnOgAdresse(FulltNavnFornavnForst("Frode Nerbråten"), Adresse("Engate 34", "1234", "Oslo")) => ()
    }
  }

  test("Brev med smsvarsling skal kunne lage og lese xml") {
    val date: DateTime = new DateTime(2012, 12, 3, 10, 0)
    Brev("id", "filnavn", "emne", Smsvarsling(Seq(date), Seq(2))) match {
      case Brev("id", "filnavn", "emne", Smsvarsling(Seq(d), Seq(2))) => assert(d === date)
    }
  }

  test("Forsendelse med Digipostadresse skal kunne lage og lese xml") {
    Forsendelse("brevid", Digipostadresse("test#1234")) match {
      case Forsendelse("brevid", Digipostadresse("test#1234"), _) => ()
    }
  }

  test("Jobbinnstillinger skal kunne lage og lese xml") {
    Jobbinstillinger("avsenderId", "jobbId", "jobbnavn", autogodkjenn = true) match {
      case Jobbinstillinger("avsenderId", "jobbId", "jobbnavn", true) => ()
    }
  }
}
