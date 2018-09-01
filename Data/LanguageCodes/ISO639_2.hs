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
language code =
  case code of
    AAR ->
    ABK ->
    ACE ->
    ACH ->
    ADA ->
    ADY ->
    AFA ->
    AFH ->
    AFR ->
    AIN ->
    AKA ->
    AKK ->
    ALB ->
    ALE ->
    ALG ->
    ALT ->
    AMH ->
    ANG ->
    ANP ->
    APA ->
    ARA ->
    ARC ->
    ARG ->
    ARM ->
    ARN ->
    ARP ->
    ART ->
    ARW ->
    ASM ->
    AST ->
    ATH ->
    AUS ->
    AVA ->
    AVE ->
    AWA ->
    AYM ->
    AZE ->
    BAD ->
    BAI ->
    BAK ->
    BAL ->
    BAM ->
    BAN ->
    BAQ ->
    BAS ->
    BAT ->
    BEJ ->
    BEL ->
    BEM ->
    BEN ->
    BER ->
    BHO ->
    BIH ->
    BIK ->
    BIN ->
    BIS ->
    BLA ->
    BNT ->
    BOD ->
    BOS ->
    BRA ->
    BRE ->
    BTK ->
    BUA ->
    BUG ->
    BUL ->
    BUR ->
    BYN ->
    CAD ->
    CAI ->
    CAR ->
    CAT ->
    CAU ->
    CEB ->
    CEL ->
    CES ->
    CHA ->
    CHB ->
    CHE ->
    CHG ->
    CHI ->
    CHK ->
    CHM ->
    CHN ->
    CHO ->
    CHP ->
    CHR ->
    CHU ->
    CHV ->
    CHY ->
    CMC ->
    CNR ->
    COP ->
    COR ->
    COS ->
    CPE ->
    CPF ->
    CPP ->
    CRE ->
    CRH ->
    CRP ->
    CSB ->
    CUS ->
    CYM ->
    CZE ->
    DAK ->
    DAN ->
    DAR ->
    DAY ->
    DEL ->
    DEN ->
    DEU ->
    DGR ->
    DIN ->
    DIV ->
    DOI ->
    DRA ->
    DSB ->
    DUA ->
    DUM ->
    DUT ->
    DYU ->
    DZO ->
    EFI ->
    EGY ->
    EKA ->
    ELL ->
    ELX ->
    ENG ->
    ENM ->
    EPO ->
    EST ->
    EUS ->
    EWE ->
    EWO ->
    FAN ->
    FAO ->
    FAS ->
    FAT ->
    FIJ ->
    FIL ->
    FIN ->
    FIU ->
    FON ->
    FRA ->
    FRE ->
    FRM ->
    FRO ->
    FRR ->
    FRS ->
    FRY ->
    FUL ->
    FUR ->
    GAA ->
    GAY ->
    GBA ->
    GEM ->
    GEO ->
    GER ->
    GEZ ->
    GIL ->
    GLA ->
    GLE ->
    GLG ->
    GLV ->
    GMH ->
    GOH ->
    GON ->
    GOR ->
    GOT ->
    GRB ->
    GRC ->
    GRE ->
    GRN ->
    GSW ->
    GUJ ->
    GWI ->
    HAI ->
    HAT ->
    HAU ->
    HAW ->
    HEB ->
    HER ->
    HIL ->
    HIM ->
    HIN ->
    HIT ->
    HMN ->
    HMO ->
    HRV ->
    HSB ->
    HUN ->
    HUP ->
    HYE ->
    IBA ->
    IBO ->
    ICE ->
    IDO ->
    III ->
    IJO ->
    IKU ->
    ILE ->
    ILO ->
    INA ->
    INC ->
    IND ->
    INE ->
    INH ->
    IPK ->
    IRA ->
    IRO ->
    ISL ->
    ITA ->
    JAV ->
    JBO ->
    JPN ->
    JPR ->
    JRB ->
    KAA ->
    KAB ->
    KAC ->
    KAL ->
    KAM ->
    KAN ->
    KAR ->
    KAS ->
    KAT ->
    KAU ->
    KAW ->
    KAZ ->
    KBD ->
    KHA ->
    KHI ->
    KHM ->
    KHO ->
    KIK ->
    KIN ->
    KIR ->
    KMB ->
    KOK ->
    KOM ->
    KON ->
    KOR ->
    KOS ->
    KPE ->
    KRC ->
    KRL ->
    KRO ->
    KRU ->
    KUA ->
    KUM ->
    KUR ->
    KUT ->
    LAD ->
    LAH ->
    LAM ->
    LAO ->
    LAT ->
    LAV ->
    LEZ ->
    LIM ->
    LIN ->
    LIT ->
    LOL ->
    LOZ ->
    LTZ ->
    LUA ->
    LUB ->
    LUG ->
    LUI ->
    LUN ->
    LUO ->
    LUS ->
    MAC ->
    MAD ->
    MAG ->
    MAH ->
    MAI ->
    MAK ->
    MAL ->
    MAN ->
    MAO ->
    MAP ->
    MAR ->
    MAS ->
    MAY ->
    MDF ->
    MDR ->
    MEN ->
    MGA ->
    MIC ->
    MIN ->
    MKD ->
    MKH ->
    MLG ->
    MLT ->
    MNC ->
    MNI ->
    MNO ->
    MOH ->
    MON ->
    MOS ->
    MRI ->
    MSA ->
    MUN ->
    MUS ->
    MWL ->
    MWR ->
    MYA ->
    MYN ->
    MYV ->
    NAH ->
    NAI ->
    NAP ->
    NAU ->
    NAV ->
    NBL ->
    NDE ->
    NDO ->
    NDS ->
    NEP ->
    NEW ->
    NIA ->
    NIC ->
    NIU ->
    NLD ->
    NNO ->
    NOB ->
    NOG ->
    NON ->
    NOR ->
    NQO ->
    NSO ->
    NUB ->
    NWC ->
    NYA ->
    NYM ->
    NYN ->
    NYO ->
    NZI ->
    OCI ->
    OJI ->
    ORI ->
    ORM ->
    OSA ->
    OSS ->
    OTA ->
    OTO ->
    PAA ->
    PAG ->
    PAL ->
    PAM ->
    PAN ->
    PAP ->
    PAU ->
    PEO ->
    PER ->
    PHI ->
    PHN ->
    PLI ->
    POL ->
    PON ->
    POR ->
    PRA ->
    PRO ->
    PUS ->
    QUE ->
    RAJ ->
    RAP ->
    RAR ->
    ROA ->
    ROH ->
    ROM ->
    RON ->
    RUM ->
    RUN ->
    RUP ->
    RUS ->
    SAD ->
    SAG ->
    SAH ->
    SAI ->
    SAL ->
    SAM ->
    SAN ->
    SAS ->
    SAT ->
    SCN ->
    SCO ->
    SEL ->
    SEM ->
    SGA ->
    SGN ->
    SHN ->
    SID ->
    SIN ->
    SIO ->
    SIT ->
    SLA ->
    SLO ->
    SLK ->
    SLV ->
    SMA ->
    SME ->
    SMI ->
    SMJ ->
    SMN ->
    SMO ->
    SMS ->
    SNA ->
    SND ->
    SNK ->
    SOG ->
    SOM ->
    SON ->
    SOT ->
    SPA ->
    SQI ->
    SRD ->
    SRN ->
    SRP ->
    SRR ->
    SSA ->
    SSW ->
    SUK ->
    SUN ->
    SUS ->
    SUX ->
    SWA ->
    SWE ->
    SYC ->
    SYR ->
    TAH ->
    TAI ->
    TAM ->
    TAT ->
    TEL ->
    TEM ->
    TER ->
    TET ->
    TGK ->
    TGL ->
    THA ->
    TIB ->
    TIG ->
    TIR ->
    TIV ->
    TKL ->
    TLH ->
    TLI ->
    TMH ->
    TOG ->
    TON ->
    TPI ->
    TSI ->
    TSN ->
    TSO ->
    TUK ->
    TUM ->
    TUP ->
    TUR ->
    TUT ->
    TVL ->
    TWI ->
    TYV ->
    UDM ->
    UGA ->
    UIG ->
    UKR ->
    UMB ->
    URD ->
    UZB ->
    VAI ->
    VEN ->
    VIE ->
    VOL ->
    VOT ->
    WAK ->
    WAL ->
    WAR ->
    WAS ->
    WEL ->
    WEN ->
    WLN ->
    WOL ->
    XAL ->
    XHO ->
    YAO ->
    YAP ->
    YID ->
    YOR ->
    YPK ->
    ZAP ->
    ZBL ->
    ZEN ->
    ZGH ->
    ZHA ->
    ZHO ->
    ZND ->
    ZUL ->
    ZUN ->
    ZZA ->