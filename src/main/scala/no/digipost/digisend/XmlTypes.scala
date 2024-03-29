package no.digipost.digisend

import xml.{Node, NodeSeq}
import org.joda.time.DateTime
import util.control.Exception.allCatch

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

  object Adresse {
    def apply(gate: String, postnummer: String, poststed: String) = {
      <adresse>
        <adresse-format1>
          <adresselinje1>{gate}</adresselinje1>
          <postnummer>{postnummer}</postnummer>
          <poststed>{poststed}</poststed>
        </adresse-format1>
      </adresse>
    }

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

  object Brev {
    def apply(id: String, filename: String, emne: String, smsVarsling: NodeSeq) = {
      <dokument xsi:type="brev">
        <id>{id}</id>
        <fil>{filename}</fil>
        <innstillinger>
          <emne>{emne}</emne>
          {smsVarsling}
        </innstillinger>
      </dokument>
    }

    def unapply(docXml: NodeSeq): Option[(String, String, String, NodeSeq)] = {
      if (docXml.toString().contains("""type="brev"""")) {
        val id = (docXml \\ "id").text
        val fil = (docXml \\ "fil").text
        val emne = (docXml \\ "emne").text
        val sms = (docXml \\ "sms-varsling")
        Some(id, fil, emne, sms)
      } else {
        None
      }
    }
  }

  object Masseutsendelse {
    def apply(jobbinnstillinger: NodeSeq, dokumenter: Seq[Node], forsendelser: Seq[Node], version: String = "1_9") = {
      val namespace = "http://www.digipost.no/xsd/avsender" + version
      <masseutsendelse xmlns={namespace} xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
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
  }

  object Jobbinstillinger {
    def apply(avsenderId: String, jobbId: String, jobbnavn: String, autogodkjenn: Boolean = false) = {
      <jobb-innstillinger>
        <avsender-id>{avsenderId}</avsender-id>
        <jobb-id>{jobbId}</jobb-id>
        <jobb-navn>{jobbnavn}</jobb-navn>
        <auto-godkjenn-jobb>{autogodkjenn}</auto-godkjenn-jobb>
      </jobb-innstillinger>
    }

    def unapply(instillingerXml: NodeSeq): Option[(String, String, String, Boolean)] = {
      val i = instillingerXml \\ "jobb-innstillinger"
      if (i.isEmpty) {
        None
      } else {
        val avsenderID = (i \\ "avsender-id").text
        val jobbId = (i \\ "jobb-id").text
        val jobbnavn = (i \\ "jobb-navn").text
        val autogodkjenn = (i \\ "auto-godkjenn-jobb").text.toBoolean
        Some(avsenderID, jobbId, jobbnavn, autogodkjenn)
      }
    }
  }

  object Smsvarsling18 {
    def apply(smsVarsling: Boolean) = <sms-varsling>{smsVarsling}</sms-varsling>

    def unapply(brevXml: NodeSeq): Option[Boolean] = allCatch.opt((brevXml \\ "sms-varsling").text.toBoolean)
  }

  object Smsvarsling {
    def apply(): NodeSeq = <sms-varsling />

    def apply(dates: Seq[DateTime], etterTimer: Seq[Int]): NodeSeq = {
      val varslinger = dates.map(d => <tidspunkt>{d.toString()}</tidspunkt>)
      val etterTimerVarslinger = etterTimer.map(t => <etter-timer>{t}</etter-timer>)
      <sms-varsling>
        {varslinger}
        {etterTimerVarslinger}
      </sms-varsling>
    }

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

  object Forsendelse {
    def apply(brevId: String, mottaker: NodeSeq, print: NodeSeq): Node = {
      <forsendelse>
        <brev>{brevId}</brev>
        {mottaker}
        {print}
      </forsendelse>
    }
    def apply(brevId: String, mottaker: NodeSeq): Node = apply(brevId, mottaker, null)

    def unapply(forsendelseXml: NodeSeq): Option[(String, NodeSeq, NodeSeq)] = {
      val f = forsendelseXml \\ "forsendelse"
      if (f.isEmpty) {
        None
      } else {
        val brevId = (f \\ "brev").text
        val mottaker = (f \\ "mottaker")
        val print = (f \\ "fysisk-print")
        Some(brevId, mottaker, print)
      }
    }
  }
}
