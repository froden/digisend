package no.digipost.digisend

import xml.{Node, NodeSeq}
import org.joda.time.DateTime

object XmlTypes {

  object Digipostadresse {
    def apply(digipostadresse: String) = {
      <mottaker>
        <kunde-id>kundeid: {digipostadresse}</kunde-id>
        <digipostadresse>{digipostadresse}</digipostadresse>
      </mottaker>
    }

    def unapply(mottakerXml: NodeSeq): Option[String] = {
      val digipostadresse = mottakerXml \\ "digipostadresse"
      if (digipostadresse.isEmpty) None else Some(digipostadresse.text)
    }
  }

  object NavnOgAdresse {
    def apply(navn: NodeSeq, adresse: NodeSeq, kundeid: String = "kundeid") = {
      <mottaker>
        <kunde-id>{kundeid}</kunde-id>
        {navn}
        {adresse}
      </mottaker>
    }

    def unapply(mottakerXml: NodeSeq): Option[(NodeSeq, NodeSeq)] = {
      val navn = mottakerXml \\ "navn"
      val adresse = mottakerXml \\ "adresse"
      if (navn.isEmpty || adresse.isEmpty) None else Some(navn, adresse)
    }
  }

  object SmsVarsling {
    def unapply(brevXml: NodeSeq): Option[(Seq[DateTime], Seq[Int])] = {
      val smsVarslingXml = brevXml \\ "sms-varsling"
      if (smsVarslingXml.isEmpty) {
        None
      } else {
        val tidspunkter = (smsVarslingXml \\ "tidspunkt").map(sms => new DateTime(sms.text))
        val etterTimer = (smsVarslingXml \\ "etter-timer").map(_.text.toInt)
        Some(tidspunkter, etterTimer)
      }
    }
  }

  object FulltNavnFornavnForst {
    def apply(navn: String) =
      <navn>
        <navn-format1>
          <fullt-navn-fornavn-foerst>{navn}</fullt-navn-fornavn-foerst>
        </navn-format1>
      </navn>

    def unapply(forsendelse: NodeSeq): Option[String] = {
      val navn = forsendelse \\ "fullt-navn-fornavn-foerst"
      if (navn.isEmpty) {
        None
      } else {
        Some(navn.text)
      }
    }
  }

  object Dokument {
    def unapply(docXml: NodeSeq): Option[(String, String)] = {
      val fil = (docXml \\ "fil").text
      val emne = (docXml \\ "emne").text
      Some(fil, emne)
    }
  }

  object Adresse {
    def apply(gate: String, postnummer: String, poststed: String): NodeSeq = adresse(gate, postnummer, poststed)

    def unapply(adrXml: NodeSeq): Option[(String, String, String)] = {
      val adresseformat1 = adrXml \\ "adresse-format1"
      if (adresseformat1.isEmpty) {
        None
      } else {
        val adresse = (adresseformat1 \\ "adresselinje1").text
        val postnummer = (adresseformat1 \\ "postnummer").text
        val poststed = (adresseformat1 \\ "poststed").text
        Some(adresse, postnummer, poststed)
      }
    }
  }

  def masseutsendelse(jobbinnstillinger: NodeSeq, dokumenter: Seq[Node], forsendelser: Seq[Node]) = {
    <masseutsendelse xmlns="http://www.digipost.no/xsd/avsender1_9" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      {jobbinnstillinger}
      <standard-distribusjon>
        <post>
          {dokumenter}
        </post>
        <forsendelser>
          {forsendelser}
        </forsendelser>
      </standard-distribusjon>
    </masseutsendelse>
  }

  def jobbinstillinger(senderId: String, jobbnavn: String) = {
    <jobb-innstillinger>
      <avsender-id>{senderId}</avsender-id>
      <jobb-id>jobbid{(math.random * 100000).toInt}</jobb-id>
      <jobb-navn>{jobbnavn}</jobb-navn>
      <auto-godkjenn-jobb>false</auto-godkjenn-jobb>
    </jobb-innstillinger>
  }

  def brev(id: String, filename: String, emne: String, smsVarsling: NodeSeq) = {
    <dokument xsi:type="brev">
      <id>{id}</id>
      <fil>{filename}</fil>
      <innstillinger>
        <emne>{emne}</emne>
        {smsVarsling}
      </innstillinger>
    </dokument>
  }

  def smsVarsling(dates: Seq[DateTime], etterTimer: Seq[Int]) = {
    val varslinger = dates.map(d => <tidspunkt>{d.toString()}</tidspunkt>)
    val etterTimerVarslinger = etterTimer.map(t => <etter-timer>{t}</etter-timer>)
    <sms-varsling>
      {varslinger}
      {etterTimerVarslinger}
    </sms-varsling>
  }

  def print(adresse: NodeSeq, returadresse: NodeSeq) = {
    <fysisk-print>
      {adresse}
      {returadresse}
    </fysisk-print>
  }

  def postadresse(navn: String, detaljer: NodeSeq) = {
    <postmottaker>{navn}</postmottaker>
      <norsk-mottakeradresse>
        {detaljer}
      </norsk-mottakeradresse>
  }

  def returadresse(navn: String, detaljer: NodeSeq) = {
    <retur-postmottaker>{navn}</retur-postmottaker>
      <norsk-returadresse>
        {detaljer}
      </norsk-returadresse>
  }

  def adressedetaljer(adresse: String, postnummer: String, poststed: String) = {
    <adresselinje1>{adresse}</adresselinje1>
      <postnummer>{postnummer}</postnummer>
      <poststed>{poststed}</poststed>
  }

  def forsendelse(brevId: String, mottaker: NodeSeq, print: NodeSeq) = {
    <forsendelse>
      <brev>{brevId}</brev>
      {mottaker}
      {print}
    </forsendelse>
  }


  def mottaker(navn: String, adresse: NodeSeq) = {
    <mottaker>
      <kunde-id>{navn}</kunde-id>
      <navn>
        <navn-format1>
          <fullt-navn-fornavn-foerst>{navn}</fullt-navn-fornavn-foerst>
        </navn-format1>
      </navn>
      {adresse}
    </mottaker>
  }

  def adresse(gate: String, postnummer: String, poststed: String) = {
    <adresse>
      <adresse-format1>
        <adresselinje1>{gate}</adresselinje1>
        <postnummer>{postnummer}</postnummer>
        <poststed>{poststed}</poststed>
      </adresse-format1>
    </adresse>
  }
}
