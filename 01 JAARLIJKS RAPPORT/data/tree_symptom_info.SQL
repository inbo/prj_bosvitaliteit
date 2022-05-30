select
  m.WRME_ID as MetingKey
, sym.WMSY_ATDE_CDE AangetastDeelCode
, cad.ATDE_DES AangetastDeel
, sym.WMSY_SYSP_CDE SymptoomSpecCode
, css.SYSP_DES SymptoomSpecificatie
, sym.WMSY_SYTO_CDE SymptoomCode
, csym.SYTO_DES Symptoom
, sym.WMSY_UCME_CDE SymptoomMeting
, sym.WMSY_LEFT_CDE LeeftijdSchadeCode
, clft.LEFT_DES as LeeftijdSchade
, sym.WMSY_OPM SymptoomOpmerking
, oorz.OORZ_CDE SymptoomOorzaakCode
, oorz.OORZ_DES SymptoomOorzaak
, ozor.OZOR_CDE SymptoomOrganismeCode
, ozor.OZOR_NAM SymptoomOrganisme
from
tblProefvlak p left join
tblWaarneming w on w.WRNG_PLOT_ID = p.PLOT_ID left join
tblWaarnemingMeting m on m.WRME_WRNG_ID = w.WRNG_ID left join
tblWaarnemingMetingSymptoom sym on sym.WMSY_WRME_ID = m.WRME_ID left join
tblBoom b on b.BOOM_ID = m.WRME_BOOM_ID left join
tblSoort s on s.SPEC_ID = b.BOOM_SPEC_ID  left join
cdeAangetastDeel cad on cad.ATDE_CDE = sym.WMSY_ATDE_CDE left join
cdeSymptoomSpecificatie css on css.SYSP_CDE = sym.WMSY_SYSP_CDE left join
cdeSymptoom csym on csym.SYTO_CDE = sym.WMSY_SYTO_CDE left join
cdeLeeftijdSchade clft on clft.LEFT_CDE = sym.WMSY_LEFT_CDE left join
tblMetingSymptoomOorzaak syo on syo.MSYO_WMSY_ID = sym.WMSY_ID left join
tblOorzaak oorz on oorz.OORZ_ID = syo.MSYO_OORZ_ID left join
tblOorzaakOrganisme ozor on ozor.OZOR_ID = syo.MSYO_OZOR_ID
