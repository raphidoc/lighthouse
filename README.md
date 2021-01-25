# ![logo](/images/logo.png) lighthouse
Set of tools for biogeochemioptic data processing and management, which primary goal is to harmonize the five package:
- [asd](https://srscm03.uqar.ca/belasi01/asd) (ASD processing)
- [Cops](https://srscm03.uqar.ca/belasi01/Cops) (C-OPS processing)
- [HyperocR](https://srscm03.uqar.ca/belasi01/HyperocR) (HOCR processing)
- [Riops](https://srscm03.uqar.ca/belasi01/Riops) (Optic package processing)
- [RspectroAbs](https://srscm03.uqar.ca/mabr0002/RspectroAbs) (laboratory spectrophotometer processing)

This harmonization pass by:
1. standardized nomenclature
2. standardized output
3. improved input/output pipeline (between packages and in-package)
4. shared functionality (remove redundancy)  

It improve efficiency, maintainability and reduce development time.  

lighthouse offer new possibility for data management with automated functions:
1. `l2_*_gen`: can create a new L2 structure from raw data files and a data synthesis table.
2. `l3_*_gen`: can create databases tables (minimum is .csv for interoperability) from processed files.
3. `sql_*`: can create a SQLite database for a specific project or merge several SQLite databases together.

In addition, automation of specific processing log and quality check procedure (`qc_*`) are implemented, allowing seamless documentation of the processing process (pun apart, the human factor in processing is more 'visible' with visual analytic methods coupled with proper documentation from the human factor).
It offer a complete processing pipeline to pass from acquisition to analysis in less time and with more reliability, giving more confidence in the data to researchers.

## Specifications

The specifications enforced here are described in [COSMOS](https://srscm03.uqar.ca/mabr0002/cosmos). API description is to be found under [/man](/man).

## <a name="Versioning"></a> Versioning

This project uses [semantic versioning](https://semver.org/). During initial development phase everything can be subject to changes, once it reach version 1.0.0 incompatible change will need to go to 2.0.0.
