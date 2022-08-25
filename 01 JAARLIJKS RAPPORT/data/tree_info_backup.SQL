select
  p.PLOT_ID as PlotKey
, b.BOOM_ID as BoomKey
, w.WRNG_ID as WaarnemingKey
, m.WRME_ID as MetingKey
, p.PLOT_NUM as PlotNr
, p.PLOT_NAM as PlotNaam
, g.GMTE_NAM as Gemeente
, b.BOOM_BNR as BoomNr
, w.WRNG_DTE as Datum
, w.WRNG_JAA as Jaar
, s.SPEC_EUR_CDE as Soortnummer
, m.WRME_OMT as Omtrek
, m.WRME_LFT as Leeftijd
, m.WRME_UCLT_CDE AS Leeftijdsklasse
, m.WRME_UCBL_CDE as BladverliesNetto
, m.WRME_OPM as MetingOpmerking
, m.WRME_UCNJ_CDE AS AantalNaaldjaargangen
, m.WRME_UCZZ_CDE AS Zaadzetting
, m.WRME_UCWS_CDE AS Waterscheuten
, b.BOOM_JDO as SterfteJaar
, b.BOOM_REMO_CDE VerwijderdCode
, cdem.REMO_DES as VerwijderdReden
from
tblProefvlak p left join
tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID left join
cdeRemovalsMortality cdem on b.BOOM_REMO_CDE = cdem.REMO_CDE left join
tblGemeente g on g.GMTE_NIS_CDE = p.PLOT_GMTE_NIS_CDE
