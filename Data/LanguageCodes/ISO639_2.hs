{-
  Notes:
    - several codes for the same language
    - same code for several languages
    - qaa-qtz -> reserved for local use
    - zxx -> not applicable
    - mis, for "uncoded languages";
    - mul, for "multiple languages";
-}

data ISO639_2 =
  | AAR
  | ABK
  | ACE
  | ACH
  | ADA
  | ADY
  | AFA
  | AFH
  | AFR
  | AIN
  | AKA
  | AKK
  | ALB
  | ALE
  | ALG
  | ALT
  | AMH
  | ANG
  | ANP
  | APA
  | ARA
  | ARC
  | ARG
  | ARM
  | ARN
  | ARP
  | ART
  | ARW
  | ASM
  | AST
  | ATH
  | AUS
  | AVA
  | AVE
  | AWA
  | AYM
  | AZE
  | BAD
  | BAI
  | BAK
  | BAL
  | BAM
  | BAN
  | BAQ
  | BAS
  | BAT
  | BEJ
  | BEL
  | BEM
  | BEN
  | BER
  | BHO
  | BIH
  | BIK
  | BIN
  | BIS
  | BLA
  | BNT
  | BOD 
  | BOS
  | BRA
  | BRE
  | BTK
  | BUA
  | BUG
  | BUL
  | BUR
  | BYN
  | CAD
  | CAI
  | CAR
  | CAT
  | CAU
  | CEB
  | CEL
  | CES 
  | CHA
  | CHB
  | CHE
  | CHG
  | CHI
  | CHK
  | CHM
  | CHN
  | CHO
  | CHP
  | CHR
  | CHU
  | CHV
  | CHY
  | CMC
  | CNR
  | COP
  | COR
  | COS
  | CPE
  | CPF
  | CPP
  | CRE
  | CRH
  | CRP
  | CSB
  | CUS
  | CYM 
  | CZE
  | DAK
  | DAN
  | DAR
  | DAY
  | DEL
  | DEN
  | DEU 
  | DGR
  | DIN
  | DIV
  | DOI
  | DRA
  | DSB
  | DUA
  | DUM
  | DUT
  | DYU
  | DZO
  | EFI
  | EGY
  | EKA
  | ELL 
  | ELX
  | ENG
  | ENM
  | EPO
  | EST
  | EUS 
  | EWE
  | EWO
  | FAN
  | FAO
  | FAS 
  | FAT
  | FIJ
  | FIL
  | FIN
  | FIU
  | FON
  | FRA 
  | FRE
  | FRM
  | FRO
  | FRR
  | FRS
  | FRY
  | FUL
  | FUR
  | GAA
  | GAY
  | GBA
  | GEM
  | GEO
  | GER
  | GEZ
  | GIL
  | GLA
  | GLE
  | GLG
  | GLV
  | GMH
  | GOH
  | GON
  | GOR
  | GOT
  | GRB
  | GRC
  | GRE
  | GRN
  | GSW
  | GUJ
  | GWI
  | HAI
  | HAT
  | HAU
  | HAW
  | HEB
  | HER
  | HIL
  | HIM
  | HIN
  | HIT
  | HMN
  | HMO
  | HRV
  | HSB
  | HUN
  | HUP
  | HYE 
  | IBA
  | IBO
  | ICE
  | IDO
  | III
  | IJO
  | IKU
  | ILE
  | ILO
  | INA
  | INC
  | IND
  | INE
  | INH
  | IPK
  | IRA
  | IRO
  | ISL 
  | ITA
  | JAV
  | JBO
  | JPN
  | JPR
  | JRB
  | KAA
  | KAB
  | KAC
  | KAL
  | KAM
  | KAN
  | KAR
  | KAS
  | KAT 
  | KAU
  | KAW
  | KAZ
  | KBD
  | KHA
  | KHI
  | KHM
  | KHO
  | KIK
  | KIN
  | KIR
  | KMB
  | KOK
  | KOM
  | KON
  | KOR
  | KOS
  | KPE
  | KRC
  | KRL
  | KRO
  | KRU
  | KUA
  | KUM
  | KUR
  | KUT
  | LAD
  | LAH
  | LAM
  | LAO
  | LAT
  | LAV
  | LEZ
  | LIM
  | LIN
  | LIT
  | LOL
  | LOZ
  | LTZ
  | LUA
  | LUB
  | LUG
  | LUI
  | LUN
  | LUO
  | LUS
  | MAC
  | MAD
  | MAG
  | MAH
  | MAI
  | MAK
  | MAL
  | MAN
  | MAO
  | MAP
  | MAR
  | MAS
  | MAY
  | MDF
  | MDR
  | MEN
  | MGA
  | MIC
  | MIN
  | MKD 
  | MKH
  | MLG
  | MLT
  | MNC
  | MNI
  | MNO
  | MOH
  | MON
  | MOS
  | MRI 
  | MSA 
  | MUN
  | MUS
  | MWL
  | MWR
  | MYA 
  | MYN
  | MYV
  | NAH
  | NAI
  | NAP
  | NAU
  | NAV
  | NBL
  | NDE
  | NDO
  | NDS
  | NEP
  | NEW
  | NIA
  | NIC
  | NIU
  | NLD 
  | NNO
  | NOB
  | NOG
  | NON
  | NOR
  | NQO
  | NSO
  | NUB
  | NWC
  | NYA
  | NYM
  | NYN
  | NYO
  | NZI
  | OCI
  | OJI
  | ORI
  | ORM
  | OSA
  | OSS
  | OTA
  | OTO
  | PAA
  | PAG
  | PAL
  | PAM
  | PAN
  | PAP
  | PAU
  | PEO
  | PER
  | PHI
  | PHN
  | PLI
  | POL
  | PON
  | POR
  | PRA
  | PRO
  | PUS
  | QUE
  | RAJ
  | RAP
  | RAR
  | ROA
  | ROH
  | ROM
  | RON 
  | RUM
  | RUN
  | RUP
  | RUS
  | SAD
  | SAG
  | SAH
  | SAI
  | SAL
  | SAM
  | SAN
  | SAS
  | SAT
  | SCN
  | SCO
  | SEL
  | SEM
  | SGA
  | SGN
  | SHN
  | SID
  | SIN
  | SIO
  | SIT
  | SLA
  | SLO
  | SLK 
  | SLV
  | SMA
  | SME
  | SMI
  | SMJ
  | SMN
  | SMO
  | SMS
  | SNA
  | SND
  | SNK
  | SOG
  | SOM
  | SON
  | SOT
  | SPA
  | SQI 
  | SRD
  | SRN
  | SRP
  | SRR
  | SSA
  | SSW
  | SUK
  | SUN
  | SUS
  | SUX
  | SWA
  | SWE
  | SYC
  | SYR
  | TAH
  | TAI
  | TAM
  | TAT
  | TEL
  | TEM
  | TER
  | TET
  | TGK
  | TGL
  | THA
  | TIB
  | TIG
  | TIR
  | TIV
  | TKL
  | TLH
  | TLI
  | TMH
  | TOG
  | TON
  | TPI
  | TSI
  | TSN
  | TSO
  | TUK
  | TUM
  | TUP
  | TUR
  | TUT
  | TVL
  | TWI
  | TYV
  | UDM
  | UGA
  | UIG
  | UKR
  | UMB
  | URD
  | UZB
  | VAI
  | VEN
  | VIE
  | VOL
  | VOT
  | WAK
  | WAL
  | WAR
  | WAS
  | WEL
  | WEN
  | WLN
  | WOL
  | XAL
  | XHO
  | YAO
  | YAP
  | YID
  | YOR
  | YPK
  | ZAP
  | ZBL
  | ZEN
  | ZGH
  | ZHA
  | ZHO 
  | ZND
  | ZUL
  | ZUN
  | ZZA

data ISO639_2_Chars = OneCode (Char, Char, Char)
                      | TwoCodes (Char, Char, Char) (Char, Char, Char)

firstCode :: ISO639_2_Chars -> (Char, Char, Char)
firstCode (OneCode code) = code
firstCode (TwoCodes code _) = code

firstToChars :: ISO639_2 -> (Char, Char, Char)
firstToChars = firstCode . toChars

toChars :: ISO639_2 -> (Char, Char, Char)
toChars isoCode = (a, b, c)
  where [a,b,c] = show isoCode

fromChars :: (Char, Char, Char) -> Maybe ISO639_2
fromChars chars =
  case chars of
    ('a', 'a', 'r') -> AAR
    ('a', 'b', 'k') -> ABK
    ('a', 'c', 'e') -> ACE
    ('a', 'c', 'h') -> ACH
    ('a', 'd', 'a') -> ADA
    ('a', 'd', 'y') -> ADY
    ('a', 'f', 'a') -> AFA
    ('a', 'f', 'h') -> AFH
    ('a', 'f', 'r') -> AFR
    ('a', 'i', 'n') -> AIN
    ('a', 'k', 'a') -> AKA
    ('a', 'k', 'k') -> AKK
    ('a', 'l', 'b') -> ALB
    ('a', 'l', 'e') -> ALE
    ('a', 'l', 'g') -> ALG
    ('a', 'l', 't') -> ALT
    ('a', 'm', 'h') -> AMH
    ('a', 'n', 'g') -> ANG
    ('a', 'n', 'p') -> ANP
    ('a', 'p', 'a') -> APA
    ('a', 'r', 'a') -> ARA
    ('a', 'r', 'c') -> ARC
    ('a', 'r', 'g') -> ARG
    ('a', 'r', 'm') -> ARM
    ('a', 'r', 'n') -> ARN
    ('a', 'r', 'p') -> ARP
    ('a', 'r', 't') -> ART
    ('a', 'r', 'w') -> ARW
    ('a', 's', 'm') -> ASM
    ('a', 's', 't') -> AST
    ('a', 't', 'h') -> ATH
    ('a', 'u', 's') -> AUS
    ('a', 'v', 'a') -> AVA
    ('a', 'v', 'e') -> AVE
    ('a', 'w', 'a') -> AWA
    ('a', 'y', 'm') -> AYM
    ('a', 'z', 'e') -> AZE
    ('b', 'a', 'd') -> BAD
    ('b', 'a', 'i') -> BAI
    ('b', 'a', 'k') -> BAK
    ('b', 'a', 'l') -> BAL
    ('b', 'a', 'm') -> BAM
    ('b', 'a', 'n') -> BAN
    ('b', 'a', 'q') -> BAQ
    ('b', 'a', 's') -> BAS
    ('b', 'a', 't') -> BAT
    ('b', 'e', 'j') -> BEJ
    ('b', 'e', 'l') -> BEL
    ('b', 'e', 'm') -> BEM
    ('b', 'e', 'n') -> BEN
    ('b', 'e', 'r') -> BER
    ('b', 'h', 'o') -> BHO
    ('b', 'i', 'h') -> BIH
    ('b', 'i', 'k') -> BIK
    ('b', 'i', 'n') -> BIN
    ('b', 'i', 's') -> BIS
    ('b', 'l', 'a') -> BLA
    ('b', 'n', 't') -> BNT
    ('b', 'o', 'd') -> BOD 
    ('b', 'o', 's') -> BOS
    ('b', 'r', 'a') -> BRA
    ('b', 'r', 'e') -> BRE
    ('b', 't', 'k') -> BTK
    ('b', 'u', 'a') -> BUA
    ('b', 'u', 'g') -> BUG
    ('b', 'u', 'l') -> BUL
    ('b', 'u', 'r') -> BUR
    ('b', 'y', 'n') -> BYN
    ('c', 'a', 'd') -> CAD
    ('c', 'a', 'i') -> CAI
    ('c', 'a', 'r') -> CAR
    ('c', 'a', 't') -> CAT
    ('c', 'a', 'u') -> CAU
    ('c', 'e', 'b') -> CEB
    ('c', 'e', 'l') -> CEL
    ('c', 'e', 's') -> CES 
    ('c', 'h', 'a') -> CHA
    ('c', 'h', 'b') -> CHB
    ('c', 'h', 'e') -> CHE
    ('c', 'h', 'g') -> CHG
    ('c', 'h', 'i') -> CHI
    ('c', 'h', 'k') -> CHK
    ('c', 'h', 'm') -> CHM
    ('c', 'h', 'n') -> CHN
    ('c', 'h', 'o') -> CHO
    ('c', 'h', 'p') -> CHP
    ('c', 'h', 'r') -> CHR
    ('c', 'h', 'u') -> CHU
    ('c', 'h', 'v') -> CHV
    ('c', 'h', 'y') -> CHY
    ('c', 'm', 'c') -> CMC
    ('c', 'n', 'r') -> CNR
    ('c', 'o', 'p') -> COP
    ('c', 'o', 'r') -> COR
    ('c', 'o', 's') -> COS
    ('c', 'p', 'e') -> CPE
    ('c', 'p', 'f') -> CPF
    ('c', 'p', 'p') -> CPP
    ('c', 'r', 'e') -> CRE
    ('c', 'r', 'h') -> CRH
    ('c', 'r', 'p') -> CRP
    ('c', 's', 'b') -> CSB
    ('c', 'u', 's') -> CUS
    ('c', 'y', 'm') -> CYM 
    ('c', 'z', 'e') -> CZE
    ('d', 'a', 'k') -> DAK
    ('d', 'a', 'n') -> DAN
    ('d', 'a', 'r') -> DAR
    ('d', 'a', 'y') -> DAY
    ('d', 'e', 'l') -> DEL
    ('d', 'e', 'n') -> DEN
    ('d', 'e', 'u') -> DEU 
    ('d', 'g', 'r') -> DGR
    ('d', 'i', 'n') -> DIN
    ('d', 'i', 'v') -> DIV
    ('d', 'o', 'i') -> DOI
    ('d', 'r', 'a') -> DRA
    ('d', 's', 'b') -> DSB
    ('d', 'u', 'a') -> DUA
    ('d', 'u', 'm') -> DUM
    ('d', 'u', 't') -> DUT
    ('d', 'y', 'u') -> DYU
    ('d', 'z', 'o') -> DZO
    ('e', 'f', 'i') -> EFI
    ('e', 'g', 'y') -> EGY
    ('e', 'k', 'a') -> EKA
    ('e', 'l', 'l') -> ELL 
    ('e', 'l', 'x') -> ELX
    ('e', 'n', 'g') -> ENG
    ('e', 'n', 'm') -> ENM
    ('e', 'p', 'o') -> EPO
    ('e', 's', 't') -> EST
    ('e', 'u', 's') -> EUS 
    ('e', 'w', 'e') -> EWE
    ('e', 'w', 'o') -> EWO
    ('f', 'a', 'n') -> FAN
    ('f', 'a', 'o') -> FAO
    ('f', 'a', 's') -> FAS 
    ('f', 'a', 't') -> FAT
    ('f', 'i', 'j') -> FIJ
    ('f', 'i', 'l') -> FIL
    ('f', 'i', 'n') -> FIN
    ('f', 'i', 'u') -> FIU
    ('f', 'o', 'n') -> FON
    ('f', 'r', 'a') -> FRA 
    ('f', 'r', 'e') -> FRE
    ('f', 'r', 'm') -> FRM
    ('f', 'r', 'o') -> FRO
    ('f', 'r', 'r') -> FRR
    ('f', 'r', 's') -> FRS
    ('f', 'r', 'y') -> FRY
    ('f', 'u', 'l') -> FUL
    ('f', 'u', 'r') -> FUR
    ('g', 'a', 'a') -> GAA
    ('g', 'a', 'y') -> GAY
    ('g', 'b', 'a') -> GBA
    ('g', 'e', 'm') -> GEM
    ('g', 'e', 'o') -> GEO
    ('g', 'e', 'r') -> GER
    ('g', 'e', 'z') -> GEZ
    ('g', 'i', 'l') -> GIL
    ('g', 'l', 'a') -> GLA
    ('g', 'l', 'e') -> GLE
    ('g', 'l', 'g') -> GLG
    ('g', 'l', 'v') -> GLV
    ('g', 'm', 'h') -> GMH
    ('g', 'o', 'h') -> GOH
    ('g', 'o', 'n') -> GON
    ('g', 'o', 'r') -> GOR
    ('g', 'o', 't') -> GOT
    ('g', 'r', 'b') -> GRB
    ('g', 'r', 'c') -> GRC
    ('g', 'r', 'e') -> GRE
    ('g', 'r', 'n') -> GRN
    ('g', 's', 'w') -> GSW
    ('g', 'u', 'j') -> GUJ
    ('g', 'w', 'i') -> GWI
    ('h', 'a', 'i') -> HAI
    ('h', 'a', 't') -> HAT
    ('h', 'a', 'u') -> HAU
    ('h', 'a', 'w') -> HAW
    ('h', 'e', 'b') -> HEB
    ('h', 'e', 'r') -> HER
    ('h', 'i', 'l') -> HIL
    ('h', 'i', 'm') -> HIM
    ('h', 'i', 'n') -> HIN
    ('h', 'i', 't') -> HIT
    ('h', 'm', 'n') -> HMN
    ('h', 'm', 'o') -> HMO
    ('h', 'r', 'v') -> HRV
    ('h', 's', 'b') -> HSB
    ('h', 'u', 'n') -> HUN
    ('h', 'u', 'p') -> HUP
    ('h', 'y', 'e') -> HYE 
    ('i', 'b', 'a') -> IBA
    ('i', 'b', 'o') -> IBO
    ('i', 'c', 'e') -> ICE
    ('i', 'd', 'o') -> IDO
    ('i', 'i', 'i') -> III
    ('i', 'j', 'o') -> IJO
    ('i', 'k', 'u') -> IKU
    ('i', 'l', 'e') -> ILE
    ('i', 'l', 'o') -> ILO
    ('i', 'n', 'a') -> INA
    ('i', 'n', 'c') -> INC
    ('i', 'n', 'd') -> IND
    ('i', 'n', 'e') -> INE
    ('i', 'n', 'h') -> INH
    ('i', 'p', 'k') -> IPK
    ('i', 'r', 'a') -> IRA
    ('i', 'r', 'o') -> IRO
    ('i', 's', 'l') -> ISL 
    ('i', 't', 'a') -> ITA
    ('j', 'a', 'v') -> JAV
    ('j', 'b', 'o') -> JBO
    ('j', 'p', 'n') -> JPN
    ('j', 'p', 'r') -> JPR
    ('j', 'r', 'b') -> JRB
    ('k', 'a', 'a') -> KAA
    ('k', 'a', 'b') -> KAB
    ('k', 'a', 'c') -> KAC
    ('k', 'a', 'l') -> KAL
    ('k', 'a', 'm') -> KAM
    ('k', 'a', 'n') -> KAN
    ('k', 'a', 'r') -> KAR
    ('k', 'a', 's') -> KAS
    ('k', 'a', 't') -> KAT 
    ('k', 'a', 'u') -> KAU
    ('k', 'a', 'w') -> KAW
    ('k', 'a', 'z') -> KAZ
    ('k', 'b', 'd') -> KBD
    ('k', 'h', 'a') -> KHA
    ('k', 'h', 'i') -> KHI
    ('k', 'h', 'm') -> KHM
    ('k', 'h', 'o') -> KHO
    ('k', 'i', 'k') -> KIK
    ('k', 'i', 'n') -> KIN
    ('k', 'i', 'r') -> KIR
    ('k', 'm', 'b') -> KMB
    ('k', 'o', 'k') -> KOK
    ('k', 'o', 'm') -> KOM
    ('k', 'o', 'n') -> KON
    ('k', 'o', 'r') -> KOR
    ('k', 'o', 's') -> KOS
    ('k', 'p', 'e') -> KPE
    ('k', 'r', 'c') -> KRC
    ('k', 'r', 'l') -> KRL
    ('k', 'r', 'o') -> KRO
    ('k', 'r', 'u') -> KRU
    ('k', 'u', 'a') -> KUA
    ('k', 'u', 'm') -> KUM
    ('k', 'u', 'r') -> KUR
    ('k', 'u', 't') -> KUT
    ('l', 'a', 'd') -> LAD
    ('l', 'a', 'h') -> LAH
    ('l', 'a', 'm') -> LAM
    ('l', 'a', 'o') -> LAO
    ('l', 'a', 't') -> LAT
    ('l', 'a', 'v') -> LAV
    ('l', 'e', 'z') -> LEZ
    ('l', 'i', 'm') -> LIM
    ('l', 'i', 'n') -> LIN
    ('l', 'i', 't') -> LIT
    ('l', 'o', 'l') -> LOL
    ('l', 'o', 'z') -> LOZ
    ('l', 't', 'z') -> LTZ
    ('l', 'u', 'a') -> LUA
    ('l', 'u', 'b') -> LUB
    ('l', 'u', 'g') -> LUG
    ('l', 'u', 'i') -> LUI
    ('l', 'u', 'n') -> LUN
    ('l', 'u', 'o') -> LUO
    ('l', 'u', 's') -> LUS
    ('m', 'a', 'c') -> MAC
    ('m', 'a', 'd') -> MAD
    ('m', 'a', 'g') -> MAG
    ('m', 'a', 'h') -> MAH
    ('m', 'a', 'i') -> MAI
    ('m', 'a', 'k') -> MAK
    ('m', 'a', 'l') -> MAL
    ('m', 'a', 'n') -> MAN
    ('m', 'a', 'o') -> MAO
    ('m', 'a', 'p') -> MAP
    ('m', 'a', 'r') -> MAR
    ('m', 'a', 's') -> MAS
    ('m', 'a', 'y') -> MAY
    ('m', 'd', 'f') -> MDF
    ('m', 'd', 'r') -> MDR
    ('m', 'e', 'n') -> MEN
    ('m', 'g', 'a') -> MGA
    ('m', 'i', 'c') -> MIC
    ('m', 'i', 'n') -> MIN
    ('m', 'k', 'd') -> MKD 
    ('m', 'k', 'h') -> MKH
    ('m', 'l', 'g') -> MLG
    ('m', 'l', 't') -> MLT
    ('m', 'n', 'c') -> MNC
    ('m', 'n', 'i') -> MNI
    ('m', 'n', 'o') -> MNO
    ('m', 'o', 'h') -> MOH
    ('m', 'o', 'n') -> MON
    ('m', 'o', 's') -> MOS
    ('m', 'r', 'i') -> MRI 
    ('m', 's', 'a') -> MSA 
    ('m', 'u', 'n') -> MUN
    ('m', 'u', 's') -> MUS
    ('m', 'w', 'l') -> MWL
    ('m', 'w', 'r') -> MWR
    ('m', 'y', 'a') -> MYA 
    ('m', 'y', 'n') -> MYN
    ('m', 'y', 'v') -> MYV
    ('n', 'a', 'h') -> NAH
    ('n', 'a', 'i') -> NAI
    ('n', 'a', 'p') -> NAP
    ('n', 'a', 'u') -> NAU
    ('n', 'a', 'v') -> NAV
    ('n', 'b', 'l') -> NBL
    ('n', 'd', 'e') -> NDE
    ('n', 'd', 'o') -> NDO
    ('n', 'd', 's') -> NDS
    ('n', 'e', 'p') -> NEP
    ('n', 'e', 'w') -> NEW
    ('n', 'i', 'a') -> NIA
    ('n', 'i', 'c') -> NIC
    ('n', 'i', 'u') -> NIU
    ('n', 'l', 'd') -> NLD 
    ('n', 'n', 'o') -> NNO
    ('n', 'o', 'b') -> NOB
    ('n', 'o', 'g') -> NOG
    ('n', 'o', 'n') -> NON
    ('n', 'o', 'r') -> NOR
    ('n', 'q', 'o') -> NQO
    ('n', 's', 'o') -> NSO
    ('n', 'u', 'b') -> NUB
    ('n', 'w', 'c') -> NWC
    ('n', 'y', 'a') -> NYA
    ('n', 'y', 'm') -> NYM
    ('n', 'y', 'n') -> NYN
    ('n', 'y', 'o') -> NYO
    ('n', 'z', 'i') -> NZI
    ('o', 'c', 'i') -> OCI
    ('o', 'j', 'i') -> OJI
    ('o', 'r', 'i') -> ORI
    ('o', 'r', 'm') -> ORM
    ('o', 's', 'a') -> OSA
    ('o', 's', 's') -> OSS
    ('o', 't', 'a') -> OTA
    ('o', 't', 'o') -> OTO
    ('p', 'a', 'a') -> PAA
    ('p', 'a', 'g') -> PAG
    ('p', 'a', 'l') -> PAL
    ('p', 'a', 'm') -> PAM
    ('p', 'a', 'n') -> PAN
    ('p', 'a', 'p') -> PAP
    ('p', 'a', 'u') -> PAU
    ('p', 'e', 'o') -> PEO
    ('p', 'e', 'r') -> PER
    ('p', 'h', 'i') -> PHI
    ('p', 'h', 'n') -> PHN
    ('p', 'l', 'i') -> PLI
    ('p', 'o', 'l') -> POL
    ('p', 'o', 'n') -> PON
    ('p', 'o', 'r') -> POR
    ('p', 'r', 'a') -> PRA
    ('p', 'r', 'o') -> PRO
    ('p', 'u', 's') -> PUS
    ('q', 'u', 'e') -> QUE
    ('r', 'a', 'j') -> RAJ
    ('r', 'a', 'p') -> RAP
    ('r', 'a', 'r') -> RAR
    ('r', 'o', 'a') -> ROA
    ('r', 'o', 'h') -> ROH
    ('r', 'o', 'm') -> ROM
    ('r', 'o', 'n') -> RON 
    ('r', 'u', 'm') -> RUM
    ('r', 'u', 'n') -> RUN
    ('r', 'u', 'p') -> RUP
    ('r', 'u', 's') -> RUS
    ('s', 'a', 'd') -> SAD
    ('s', 'a', 'g') -> SAG
    ('s', 'a', 'h') -> SAH
    ('s', 'a', 'i') -> SAI
    ('s', 'a', 'l') -> SAL
    ('s', 'a', 'm') -> SAM
    ('s', 'a', 'n') -> SAN
    ('s', 'a', 's') -> SAS
    ('s', 'a', 't') -> SAT
    ('s', 'c', 'n') -> SCN
    ('s', 'c', 'o') -> SCO
    ('s', 'e', 'l') -> SEL
    ('s', 'e', 'm') -> SEM
    ('s', 'g', 'a') -> SGA
    ('s', 'g', 'n') -> SGN
    ('s', 'h', 'n') -> SHN
    ('s', 'i', 'd') -> SID
    ('s', 'i', 'n') -> SIN
    ('s', 'i', 'o') -> SIO
    ('s', 'i', 't') -> SIT
    ('s', 'l', 'a') -> SLA
    ('s', 'l', 'o') -> SLO
    ('s', 'l', 'k') -> SLK 
    ('s', 'l', 'v') -> SLV
    ('s', 'm', 'a') -> SMA
    ('s', 'm', 'e') -> SME
    ('s', 'm', 'i') -> SMI
    ('s', 'm', 'j') -> SMJ
    ('s', 'm', 'n') -> SMN
    ('s', 'm', 'o') -> SMO
    ('s', 'm', 's') -> SMS
    ('s', 'n', 'a') -> SNA
    ('s', 'n', 'd') -> SND
    ('s', 'n', 'k') -> SNK
    ('s', 'o', 'g') -> SOG
    ('s', 'o', 'm') -> SOM
    ('s', 'o', 'n') -> SON
    ('s', 'o', 't') -> SOT
    ('s', 'p', 'a') -> SPA
    ('s', 'q', 'i') -> SQI 
    ('s', 'r', 'd') -> SRD
    ('s', 'r', 'n') -> SRN
    ('s', 'r', 'p') -> SRP
    ('s', 'r', 'r') -> SRR
    ('s', 's', 'a') -> SSA
    ('s', 's', 'w') -> SSW
    ('s', 'u', 'k') -> SUK
    ('s', 'u', 'n') -> SUN
    ('s', 'u', 's') -> SUS
    ('s', 'u', 'x') -> SUX
    ('s', 'w', 'a') -> SWA
    ('s', 'w', 'e') -> SWE
    ('s', 'y', 'c') -> SYC
    ('s', 'y', 'r') -> SYR
    ('t', 'a', 'h') -> TAH
    ('t', 'a', 'i') -> TAI
    ('t', 'a', 'm') -> TAM
    ('t', 'a', 't') -> TAT
    ('t', 'e', 'l') -> TEL
    ('t', 'e', 'm') -> TEM
    ('t', 'e', 'r') -> TER
    ('t', 'e', 't') -> TET
    ('t', 'g', 'k') -> TGK
    ('t', 'g', 'l') -> TGL
    ('t', 'h', 'a') -> THA
    ('t', 'i', 'b') -> TIB
    ('t', 'i', 'g') -> TIG
    ('t', 'i', 'r') -> TIR
    ('t', 'i', 'v') -> TIV
    ('t', 'k', 'l') -> TKL
    ('t', 'l', 'h') -> TLH
    ('t', 'l', 'i') -> TLI
    ('t', 'm', 'h') -> TMH
    ('t', 'o', 'g') -> TOG
    ('t', 'o', 'n') -> TON
    ('t', 'p', 'i') -> TPI
    ('t', 's', 'i') -> TSI
    ('t', 's', 'n') -> TSN
    ('t', 's', 'o') -> TSO
    ('t', 'u', 'k') -> TUK
    ('t', 'u', 'm') -> TUM
    ('t', 'u', 'p') -> TUP
    ('t', 'u', 'r') -> TUR
    ('t', 'u', 't') -> TUT
    ('t', 'v', 'l') -> TVL
    ('t', 'w', 'i') -> TWI
    ('t', 'y', 'v') -> TYV
    ('u', 'd', 'm') -> UDM
    ('u', 'g', 'a') -> UGA
    ('u', 'i', 'g') -> UIG
    ('u', 'k', 'r') -> UKR
    ('u', 'm', 'b') -> UMB
    ('u', 'r', 'd') -> URD
    ('u', 'z', 'b') -> UZB
    ('v', 'a', 'i') -> VAI
    ('v', 'e', 'n') -> VEN
    ('v', 'i', 'e') -> VIE
    ('v', 'o', 'l') -> VOL
    ('v', 'o', 't') -> VOT
    ('w', 'a', 'k') -> WAK
    ('w', 'a', 'l') -> WAL
    ('w', 'a', 'r') -> WAR
    ('w', 'a', 's') -> WAS
    ('w', 'e', 'l') -> WEL
    ('w', 'e', 'n') -> WEN
    ('w', 'l', 'n') -> WLN
    ('w', 'o', 'l') -> WOL
    ('x', 'a', 'l') -> XAL
    ('x', 'h', 'o') -> XHO
    ('y', 'a', 'o') -> YAO
    ('y', 'a', 'p') -> YAP
    ('y', 'i', 'd') -> YID
    ('y', 'o', 'r') -> YOR
    ('y', 'p', 'k') -> YPK
    ('z', 'a', 'p') -> ZAP
    ('z', 'b', 'l') -> ZBL
    ('z', 'e', 'n') -> ZEN
    ('z', 'g', 'h') -> ZGH
    ('z', 'h', 'a') -> ZHA
    ('z', 'h', 'o') -> ZHO
    ('z', 'n', 'd') -> ZND
    ('z', 'u', 'l') -> ZUL
    ('z', 'u', 'n') -> ZUN
    ('z', 'z', 'a') -> ZZA

language :: ISO639_2 -> String
language = first . languageList

languageList :: ISO639_2 -> [String]
languageList code =
  case code of
    AAR -> ["Afar"]
    ABK -> ["Abkhazian"]
    ACE -> ["Achinese"]
    ACH -> ["Acoli"]
    ADA -> ["Adangme"]
    ADY -> ["Adyghe", "Adygei"]
    AFA -> ["Afro-Asiatic languages"]
    AFH -> ["Afrihili"]
    AFR -> ["Afrikaans"]
    AIN -> ["Ainu"]
    AKA -> ["Akan"]
    AKK -> ["Akkadian"]
    ALB -> ["Albanian"]
    ALE -> ["Aleut"]
    ALG -> ["Algonquian languages"]
    ALT -> ["Southern Altai"]
    AMH -> ["Amharic"]
    ANG -> ["Old English (ca. 450–1100)"]
    ANP -> ["Angika"]
    APA -> ["Apache languages"]
    ARA -> ["Arabic"]
    ARC -> ["Official Aramaic (700–300 BCE)", "Imperial Aramaic (700–300 BCE)"]
    ARG -> ["Aragonese"]
    ARM -> ["Armenian"]
    ARN -> ["Mapudungun", "Mapuche"]
    ARP -> ["Arapaho"]
    ART -> ["Artificial languages"]
    ARW -> ["Arawak"]
    ASM -> ["Assamese"]
    AST -> ["Asturian", "Bable", "Leonese", "Asturleonese"]
    ATH -> ["Athapascan languages"]
    AUS -> ["Australian languages"]
    AVA -> ["Avaric"]
    AVE -> ["Avestan"]
    AWA -> ["Awadhi"]
    AYM -> ["Aymara"]
    AZE -> ["Azerbaijani"]
    BAD -> ["Banda languages"]
    BAI -> ["Bamileke languages"]
    BAK -> ["Bashkir"]
    BAL -> ["Baluchi"]
    BAM -> ["Bambara"]
    BAN -> ["Balinese"]
    BAQ -> ["Basque"]
    BAS -> ["Basa"]
    BAT -> ["Baltic languages"]
    BEJ -> ["Beja", "Bedawiyet"]
    BEL -> ["Belarusian"]
    BEM -> ["Bemba"]
    BEN -> ["Bengali"]
    BER -> ["Berber languages"]
    BHO -> ["Bhojpuri"]
    BIH -> ["Bihari languages"]
    BIK -> ["Bikol"]
    BIN -> ["Bini", "Edo"]
    BIS -> ["Bislama"]
    BLA -> ["Siksika"]
    BNT -> ["Bantu languages"]
    BOD -> ["Tibetan"]
    BOS -> ["Bosnian"]
    BRA -> ["Braj"]
    BRE -> ["Breton"]
    BTK -> ["Batak languages"]
    BUA -> ["Buriat"]
    BUG -> ["Buginese"]
    BUL -> ["Bulgarian"]
    BUR -> ["Burmese"]
    BYN -> ["Bilin", "Blin"]
    CAD -> ["Caddo"]
    CAI -> ["Central American Indian languages"]
    CAR -> ["Galibi Carib"]
    CAT -> ["Catalan", "Valencian"]
    CAU -> ["Caucasian languages"]
    CEB -> ["Cebuano"]
    CEL -> ["Celtic languages"]
    CES -> ["Czech"]
    CHA -> ["Chamorro"]
    CHB -> ["Chibcha"]
    CHE -> ["Chechen"]
    CHG -> ["Chagatai"]
    CHI -> ["Chinese"]
    CHK -> ["Chuukese"]
    CHM -> ["Mari"]
    CHN -> ["Chinook jargon"]
    CHO -> ["Choctaw"]
    CHP -> ["Chipewyan", "Dene Suline"]
    CHR -> ["Cherokee"]
    CHU -> ["Church Slavic", "Old Slavonic", "Church Slavonic", "Old Bulgarian", "Old Church Slavonic"]
    CHV -> ["Chuvash"]
    CHY -> ["Cheyenne"]
    CMC -> ["Chamic languages"]
    CNR -> ["Montenegrin"]
    COP -> ["Coptic"]
    COR -> ["Cornish"]
    COS -> ["Corsican"]
    CPE -> ["English based Creoles and pidgins"]
    CPF -> ["French-based Creoles and pidgins"]
    CPP -> ["Portuguese-based Creoles and pidgins"]
    CRE -> ["Cree"]
    CRH -> ["Crimean Tatar", "Crimean Turkish"]
    CRP -> ["Creoles and pidgins"]
    CSB -> ["Kashubian"]
    CUS -> ["Cushitic languages"]
    CYM -> ["Welsh"]
    CZE -> ["Czech"]
    DAK -> ["Dakota"]
    DAN -> ["Danish"]
    DAR -> ["Dargwa"]
    DAY -> ["Land Dayak languages"]
    DEL -> ["Delaware"]
    DEN -> ["Slave (Athapascan)"]
    DEU -> ["German"]
    DGR -> ["Dogrib"]
    DIN -> ["Dinka"]
    DIV -> ["Dhivehi", "Dhivehi", "Maldivian"]
    DOI -> ["Dogri"]
    DRA -> ["Dravidian languages"]
    DSB -> ["Lower Sorbian"]
    DUA -> ["Duala"]
    DUM -> ["Middle Dutch (ca. 1050–1350)"]
    DUT -> ["Dutch", "Flemish"]
    DYU -> ["Dyula"]
    DZO -> ["Dzongkha"]
    EFI -> ["Efik"]
    EGY -> ["Egyptian (Ancient)"]
    EKA -> ["Ekajuk"]
    ELL -> ["Modern Greek (1453–)"]
    ELX -> ["Elamite"]
    ENG -> ["English"]
    ENM -> ["Middle English (1100–1500)"]
    EPO -> ["Esperanto"]
    EST -> ["Estonian"]
    EUS -> ["Basque"]
    EWE -> ["Ewe"]
    EWO -> ["Ewondo"]
    FAN -> ["Fang"]
    FAO -> ["Faroese"]
    FAS -> ["Persian"]
    FAT -> ["Fanti"]
    FIJ -> ["Fijian"]
    FIL -> ["Filipino", "Pilipino"]
    FIN -> ["Finnish"]
    FIU -> ["Finno-Ugrian languages"]
    FON -> ["Fon"]
    FRA -> ["French"]
    FRE -> ["French"]
    FRM -> ["Middle French (ca. 1400–1600)"]
    FRO -> ["Old French (842–ca. 1400)"]
    FRR -> ["Northern Frisian"]
    FRS -> ["Eastern Frisian"]
    FRY -> ["Western Frisian"]
    FUL -> ["Fulah"]
    FUR -> ["Friulian"]
    GAA -> ["Ga"]
    GAY -> ["Gayo"]
    GBA -> ["Gbaya"]
    GEM -> ["Germanic languages"]
    GEO -> ["Georgian"]
    GER -> ["German"]
    GEZ -> ["Geez"]
    GIL -> ["Gilbertese"]
    GLA -> ["Gaelic", "Scottish Gaelic"]
    GLE -> ["Irish"]
    GLG -> ["Galician"]
    GLV -> ["Manx"]
    GMH -> ["Middle High German (ca. 1050–1500)"]
    GOH -> ["Old High German (ca. 750–1050)"]
    GON -> ["Gondi"]
    GOR -> ["Gorontalo"]
    GOT -> ["Gothic"]
    GRB -> ["Grebo"]
    GRC -> ["Ancient Greek (to 1453)"]
    GRE -> ["Modern Greek (1453–)"]
    GRN -> ["Guarani"]
    GSW -> ["Swiss German", "Alemannic", "Alsatian"]
    GUJ -> ["Gujarati"]
    GWI -> ["Gwichʼin"]
    HAI -> ["Haida"]
    HAT -> ["Haitian", "Haitian Creole"]
    HAU -> ["Hausa"]
    HAW -> ["Hawaiian"]
    HEB -> ["Hebrew"]
    HER -> ["Herero"]
    HIL -> ["Hiligaynon"]
    HIM -> ["Himachali languages", "Western Pahari languages"]
    HIN -> ["Hindi"]
    HIT -> ["Hittite"]
    HMN -> ["Hmong", "Mong"]
    HMO -> ["Hiri Motu"]
    HRV -> ["Croatian"]
    HSB -> ["Upper Sorbian"]
    HUN -> ["Hungarian"]
    HUP -> ["Hupa"]
    HYE -> ["Armenian"]
    IBA -> ["Iban"]
    IBO -> ["Igbo"]
    ICE -> ["Icelandic"]
    IDO -> ["Ido"]
    III -> ["Sichuan Yi", "Nuosu"]
    IJO -> ["Ijo languages"]
    IKU -> ["Inuktitut"]
    ILE -> ["Interlingue", "Occidental"]
    ILO -> ["Iloko"]
    INA -> ["Interlingua (International Auxiliary Language Association)"]
    INC -> ["Indic languages"]
    IND -> ["Indonesian"]
    INE -> ["Indo-European languages"]
    INH -> ["Ingush"]
    IPK -> ["Inupiaq"]
    IRA -> ["Iranian languages"]
    IRO -> ["Iroquoian languages"]
    ISL -> ["Icelandic"]
    ITA -> ["Italian"]
    JAV -> ["Javanese"]
    JBO -> ["Lojban"]
    JPN -> ["Japanese"]
    JPR -> ["Judeo-Persian"]
    JRB -> ["Judeo-Arabic"]
    KAA -> ["Kara-Kalpak"]
    KAB -> ["Kabyle"]
    KAC -> ["Kachin", "Jingpho"]
    KAL -> ["Kalaallisut", "Greenlandic"]
    KAM -> ["Kamba"]
    KAN -> ["Kannada"]
    KAR -> ["Karen languages"]
    KAS -> ["Kashmiri"]
    KAT -> ["Georgian"]
    KAU -> ["Kanuri"]
    KAW -> ["Kawi"]
    KAZ -> ["Kazakh"]
    KBD -> ["Kabardian"]
    KHA -> ["Khasi"]
    KHI -> ["Khoisan languages"]
    KHM -> ["Central Khmer"]
    KHO -> ["Khotanese", "Sakan"]
    KIK -> ["Kikuyu", "Gikuyu"]
    KIN -> ["Kinyarwanda"]
    KIR -> ["Kirghiz", "Kyrgyz"]
    KMB -> ["Kimbundu"]
    KOK -> ["Konkani"]
    KOM -> ["Komi"]
    KON -> ["Kongo"]
    KOR -> ["Korean"]
    KOS -> ["Kosraean"]
    KPE -> ["Kpelle"]
    KRC -> ["Karachay-Balkar"]
    KRL -> ["Karelian"]
    KRO -> ["Kru languages"]
    KRU -> ["Kurukh"]
    KUA -> ["Kuanyama", "Kwanyama"]
    KUM -> ["Kumyk"]
    KUR -> ["Kurdish"]
    KUT -> ["Kutenai"]
    LAD -> ["Ladino"]
    LAH -> ["Lahnda"]
    LAM -> ["Lamba"]
    LAO -> ["Lao"]
    LAT -> ["Latin"]
    LAV -> ["Latvian"]
    LEZ -> ["Lezghian"]
    LIM -> ["Limburgan", "Limburger", "Limburgish"]
    LIN -> ["Lingala"]
    LIT -> ["Lithuanian"]
    LOL -> ["Mongo"]
    LOZ -> ["Lozi"]
    LTZ -> ["Luxembourgish", "Letzeburgesch"]
    LUA -> ["Luba-Lulua"]
    LUB -> ["Luba-Katanga"]
    LUG -> ["Ganda"]
    LUI -> ["Luiseno"]
    LUN -> ["Lunda"]
    LUO -> ["Luo (Kenya and Tanzania)"]
    LUS -> ["Lushai"]
    MAC -> ["Macedonian"]
    MAD -> ["Madurese"]
    MAG -> ["Magahi"]
    MAH -> ["Marshallese"]
    MAI -> ["Maithili"]
    MAK -> ["Makasar"]
    MAL -> ["Malayalam"]
    MAN -> ["Mandingo"]
    MAO -> ["Maori"]
    MAP -> ["Austronesian languages"]
    MAR -> ["Marathi"]
    MAS -> ["Masai"]
    MAY -> ["Malay"]
    MDF -> ["Moksha"]
    MDR -> ["Mandar"]
    MEN -> ["Mende"]
    MGA -> ["Middle Irish (900–1200)"]
    MIC -> ["Mi'kmaq", "Micmac"]
    MIN -> ["Minangkabau"]
    MKD -> ["Macedonian"]
    MKH -> ["Mon-Khmer languages"]
    MLG -> ["Malagasy"]
    MLT -> ["Maltese"]
    MNC -> ["Manchu"]
    MNI -> ["Manipuri"]
    MNO -> ["Manobo languages"]
    MOH -> ["Mohawk"]
    MON -> ["Mongolian"]
    MOS -> ["Mossi"]
    MRI -> ["Maori"]
    MSA -> ["Malay"]
    MUN -> ["Munda languages"]
    MUS -> ["Creek"]
    MWL -> ["Mirandese"]
    MWR -> ["Marwari"]
    MYA -> ["Burmese"]
    MYN -> ["Mayan languages"]
    MYV -> ["Erzya"]
    NAH -> ["Nahuatl languages"]
    NAI -> ["North American Indian languages"]
    NAP -> ["Neapolitan"]
    NAU -> ["Nauru"]
    NAV -> ["Navajo", "Navaho"]
    NBL -> ["South Ndebele"]
    NDE -> ["North Ndebele"]
    NDO -> ["Ndonga"]
    NDS -> ["Low German", "Low Saxon"]
    NEP -> ["Nepali"]
    NEW -> ["Nepal Bhasa", "Newari"]
    NIA -> ["Nias"]
    NIC -> ["Niger-Kordofanian languages"]
    NIU -> ["Niuean"]
    NLD -> ["Dutch", "Flemish"]
    NNO -> ["Norwegian Nynorsk"]
    NOB -> ["Norwegian Bokmål"]
    NOG -> ["Nogai"]
    NON -> ["Old Norse"]
    NOR -> ["Norwegian"]
    NQO -> ["N'Ko"]
    NSO -> ["Pedi", "Sepedi", "Northern Sotho"]
    NUB -> ["Nubian languages"]
    NWC -> ["Classical Newari", "Old Newari", "Classical Nepal Bhasa"]
    NYA -> ["Chichewa", "Chewa", "Nyanja"]
    NYM -> ["Nyamwezi"]
    NYN -> ["Nyankole"]
    NYO -> ["Nyoro"]
    NZI -> ["Nzima"]
    OCI -> ["Occitan (post 1500)"]
    OJI -> ["Ojibwa"]
    ORI -> ["Oriya"]
    ORM -> ["Oromo"]
    OSA -> ["Osage"]
    OSS -> ["Ossetian", "Ossetic"]
    OTA -> ["Ottoman Turkish (1500–1928)"]
    OTO -> ["Otomian languages"]
    PAA -> ["Papuan languages"]
    PAG -> ["Pangasinan"]
    PAL -> ["Pahlavi"]
    PAM -> ["Pampanga", "Kapampangan"]
    PAN -> ["Panjabi", "Punjabi"]
    PAP -> ["Papiamento"]
    PAU -> ["Palauan"]
    PEO -> ["Old Persian (ca. 600–400 B.C.)"]
    PER -> ["Persian"]
    PHI -> ["Philippine languages"]
    PHN -> ["Phoenician"]
    PLI -> ["Pali"]
    POL -> ["Polish"]
    PON -> ["Pohnpeian"]
    POR -> ["Portuguese"]
    PRA -> ["Prakrit languages"]
    PRO -> ["Old Provençal (to 1500)", "Old Occitan (to 1500)"]
    PUS -> ["Pushto", "Pashto"]
    QUE -> ["Quechua"]
    RAJ -> ["Rajasthani"]
    RAP -> ["Rapanui"]
    RAR -> ["Rarotongan", "Cook Islands Maori"]
    ROA -> ["Romance languages"]
    ROH -> ["Romansh"]
    ROM -> ["Romany"]
    RON -> ["Romanian", "Moldavian", "Moldovan"]
    RUM -> ["Romanian", "Moldavian", "Moldovan"]
    RUN -> ["Rundi"]
    RUP -> ["Aromanian", "Arumanian", "Macedo-Romanian"]
    RUS -> ["Russian"]
    SAD -> ["Sandawe"]
    SAG -> ["Sango"]
    SAH -> ["Yakut"]
    SAI -> ["South American Indian languages"]
    SAL -> ["Salishan languages"]
    SAM -> ["Samaritan Aramaic"]
    SAN -> ["Sanskrit"]
    SAS -> ["Sasak"]
    SAT -> ["Santali"]
    SCN -> ["Sicilian"]
    SCO -> ["Scots"]
    SEL -> ["Selkup"]
    SEM -> ["Semitic languages"]
    SGA -> ["Old Irish (to 900)"]
    SGN -> ["Sign languages"]
    SHN -> ["Shan"]
    SID -> ["Sidamo"]
    SIN -> ["Sinhala", "Sinhalese"]
    SIO -> ["Siouan languages"]
    SIT -> ["Sino-Tibetan languages"]
    SLA -> ["Slavic languages"]
    SLO -> ["Slovak"]
    SLK -> ["Slovak"]
    SLV -> ["Slovenian"]
    SMA -> ["Southern Sami"]
    SME -> ["Northern Sami"]
    SMI -> ["Sami languages"]
    SMJ -> ["Lule Sami"]
    SMN -> ["Inari Sami"]
    SMO -> ["Samoan"]
    SMS -> ["Skolt Sami"]
    SNA -> ["Shona"]
    SND -> ["Sindhi"]
    SNK -> ["Soninke"]
    SOG -> ["Sogdian"]
    SOM -> ["Somali"]
    SON -> ["Songhai languages"]
    SOT -> ["Southern Sotho"]
    SPA -> ["Spanish", "Castilian"]
    SQI -> ["Albanian"]
    SRD -> ["Sardinian"]
    SRN -> ["Sranan Tongo"]
    SRP -> ["Serbian"]
    SRR -> ["Serer"]
    SSA -> ["Nilo-Saharan languages"]
    SSW -> ["Swati"]
    SUK -> ["Sukuma"]
    SUN -> ["Sundanese"]
    SUS -> ["Susu"]
    SUX -> ["Sumerian"]
    SWA -> ["Swahili"]
    SWE -> ["Swedish"]
    SYC -> ["Classical Syriac"]
    SYR -> ["Syriac"]
    TAH -> ["Tahitian"]
    TAI -> ["Tai languages"]
    TAM -> ["Tamil"]
    TAT -> ["Tatar"]
    TEL -> ["Telugu"]
    TEM -> ["Timne"]
    TER -> ["Tereno"]
    TET -> ["Tetum"]
    TGK -> ["Tajik"]
    TGL -> ["Tagalog"]
    THA -> ["Thai"]
    TIB -> ["Tibetan"]
    TIG -> ["Tigre"]
    TIR -> ["Tigrinya"]
    TIV -> ["Tiv"]
    TKL -> ["Tokelau"]
    TLH -> ["Klingon", "tlhIngan-Hol"]
    TLI -> ["Tlingit"]
    TMH -> ["Tamashek"]
    TOG -> ["Tonga (Nyasa)"]
    TON -> ["Tonga (Tonga Islands)"]
    TPI -> ["Tok Pisin"]
    TSI -> ["Tsimshian"]
    TSN -> ["Tswana"]
    TSO -> ["Tsonga"]
    TUK -> ["Turkmen"]
    TUM -> ["Tumbuka"]
    TUP -> ["Tupi languages"]
    TUR -> ["Turkish"]
    TUT -> ["Altaic languages"]
    TVL -> ["Tuvalua"]
    TWI -> ["Twi"]
    TYV -> ["Tuvinian"]
    UDM -> ["Udmurt"]
    UGA -> ["Ugaritic"]
    UIG -> ["Uighur", "Uyghur"]
    UKR -> ["Ukrainian"]
    UMB -> ["Umbundu"]
    URD -> ["Urdu"]
    UZB -> ["Uzbek"]
    VAI -> ["Vai"]
    VEN -> ["Venda"]
    VIE -> ["Vietnamese"]
    VOL -> ["Volapük"]
    VOT -> ["Votic"]
    WAK -> ["Wakashan languages"]
    WAL -> ["Wolaitta", "Wolaytta"]
    WAR -> ["Waray"]
    WAS -> ["Washo"]
    WEL -> ["Welsh"]
    WEN -> ["Sorbian languages"]
    WLN -> ["Walloon"]
    WOL -> ["Wolof"]
    XAL -> ["Kalmyk", "Oirat"]
    XHO -> ["Xhosa"]
    YAO -> ["Yao"]
    YAP -> ["Yapese"]
    YID -> ["Yiddish"]
    YOR -> ["Yoruba"]
    YPK -> ["Yupik languages"]
    ZAP -> ["Zapotec"]
    ZBL -> ["Blissymbols", "Blissymbolics", "Bliss"]
    ZEN -> ["Zenaga"]
    ZGH -> ["Standard Moroccan Tamazight"]
    ZHA -> ["Zhuang", "Chuang"]
    ZHO -> ["Chinese"]
    ZND -> ["Zande languages"]
    ZUL -> ["Zulu"]
    ZUN -> ["Zuni"]
    ZZA -> ["Zaza", "Dimili", "Dimli", "Kirdki", "Kirmanjki", "Zazaki"]