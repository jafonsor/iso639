
module Data.LanguageCodes.ISO639_2 where

import Data.Char (toLower)
import Data.List ()
import Prelude hiding (Ordering(..))

{-
  Notes:
    - several codes for the same language
    - same code for several languages
    - qaa-qtz reserved for local use
    - zxx -> not applicable
    - mis, for "uncoded languages";
    - mul, for "multiple languages";
-}

data ISO639_2 =
    AAR
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
  deriving (Show, Read, Eq, Enum, Ord) 

data ISO639_2_Chars = OneCode (Char, Char, Char)
                      | TwoCodes (Char, Char, Char) (Char, Char, Char)

toChars :: ISO639_2 -> (Char, Char, Char)
toChars isoCode = (a, b, c)
  where [a,b,c] = map (toLower) $ show isoCode

fromChars :: (Char, Char, Char) -> Maybe ISO639_2
fromChars chars =
  case chars of
    ('a', 'a', 'r') -> Just AAR
    ('a', 'b', 'k') -> Just ABK
    ('a', 'c', 'e') -> Just ACE
    ('a', 'c', 'h') -> Just ACH
    ('a', 'd', 'a') -> Just ADA
    ('a', 'd', 'y') -> Just ADY
    ('a', 'f', 'a') -> Just AFA
    ('a', 'f', 'h') -> Just AFH
    ('a', 'f', 'r') -> Just AFR
    ('a', 'i', 'n') -> Just AIN
    ('a', 'k', 'a') -> Just AKA
    ('a', 'k', 'k') -> Just AKK
    ('a', 'l', 'b') -> Just ALB
    ('a', 'l', 'e') -> Just ALE
    ('a', 'l', 'g') -> Just ALG
    ('a', 'l', 't') -> Just ALT
    ('a', 'm', 'h') -> Just AMH
    ('a', 'n', 'g') -> Just ANG
    ('a', 'n', 'p') -> Just ANP
    ('a', 'p', 'a') -> Just APA
    ('a', 'r', 'a') -> Just ARA
    ('a', 'r', 'c') -> Just ARC
    ('a', 'r', 'g') -> Just ARG
    ('a', 'r', 'm') -> Just ARM
    ('a', 'r', 'n') -> Just ARN
    ('a', 'r', 'p') -> Just ARP
    ('a', 'r', 't') -> Just ART
    ('a', 'r', 'w') -> Just ARW
    ('a', 's', 'm') -> Just ASM
    ('a', 's', 't') -> Just AST
    ('a', 't', 'h') -> Just ATH
    ('a', 'u', 's') -> Just AUS
    ('a', 'v', 'a') -> Just AVA
    ('a', 'v', 'e') -> Just AVE
    ('a', 'w', 'a') -> Just AWA
    ('a', 'y', 'm') -> Just AYM
    ('a', 'z', 'e') -> Just AZE
    ('b', 'a', 'd') -> Just BAD
    ('b', 'a', 'i') -> Just BAI
    ('b', 'a', 'k') -> Just BAK
    ('b', 'a', 'l') -> Just BAL
    ('b', 'a', 'm') -> Just BAM
    ('b', 'a', 'n') -> Just BAN
    ('b', 'a', 'q') -> Just BAQ
    ('b', 'a', 's') -> Just BAS
    ('b', 'a', 't') -> Just BAT
    ('b', 'e', 'j') -> Just BEJ
    ('b', 'e', 'l') -> Just BEL
    ('b', 'e', 'm') -> Just BEM
    ('b', 'e', 'n') -> Just BEN
    ('b', 'e', 'r') -> Just BER
    ('b', 'h', 'o') -> Just BHO
    ('b', 'i', 'h') -> Just BIH
    ('b', 'i', 'k') -> Just BIK
    ('b', 'i', 'n') -> Just BIN
    ('b', 'i', 's') -> Just BIS
    ('b', 'l', 'a') -> Just BLA
    ('b', 'n', 't') -> Just BNT
    ('b', 'o', 'd') -> Just BOD 
    ('b', 'o', 's') -> Just BOS
    ('b', 'r', 'a') -> Just BRA
    ('b', 'r', 'e') -> Just BRE
    ('b', 't', 'k') -> Just BTK
    ('b', 'u', 'a') -> Just BUA
    ('b', 'u', 'g') -> Just BUG
    ('b', 'u', 'l') -> Just BUL
    ('b', 'u', 'r') -> Just BUR
    ('b', 'y', 'n') -> Just BYN
    ('c', 'a', 'd') -> Just CAD
    ('c', 'a', 'i') -> Just CAI
    ('c', 'a', 'r') -> Just CAR
    ('c', 'a', 't') -> Just CAT
    ('c', 'a', 'u') -> Just CAU
    ('c', 'e', 'b') -> Just CEB
    ('c', 'e', 'l') -> Just CEL
    ('c', 'e', 's') -> Just CES 
    ('c', 'h', 'a') -> Just CHA
    ('c', 'h', 'b') -> Just CHB
    ('c', 'h', 'e') -> Just CHE
    ('c', 'h', 'g') -> Just CHG
    ('c', 'h', 'i') -> Just CHI
    ('c', 'h', 'k') -> Just CHK
    ('c', 'h', 'm') -> Just CHM
    ('c', 'h', 'n') -> Just CHN
    ('c', 'h', 'o') -> Just CHO
    ('c', 'h', 'p') -> Just CHP
    ('c', 'h', 'r') -> Just CHR
    ('c', 'h', 'u') -> Just CHU
    ('c', 'h', 'v') -> Just CHV
    ('c', 'h', 'y') -> Just CHY
    ('c', 'm', 'c') -> Just CMC
    ('c', 'n', 'r') -> Just CNR
    ('c', 'o', 'p') -> Just COP
    ('c', 'o', 'r') -> Just COR
    ('c', 'o', 's') -> Just COS
    ('c', 'p', 'e') -> Just CPE
    ('c', 'p', 'f') -> Just CPF
    ('c', 'p', 'p') -> Just CPP
    ('c', 'r', 'e') -> Just CRE
    ('c', 'r', 'h') -> Just CRH
    ('c', 'r', 'p') -> Just CRP
    ('c', 's', 'b') -> Just CSB
    ('c', 'u', 's') -> Just CUS
    ('c', 'y', 'm') -> Just CYM 
    ('c', 'z', 'e') -> Just CZE
    ('d', 'a', 'k') -> Just DAK
    ('d', 'a', 'n') -> Just DAN
    ('d', 'a', 'r') -> Just DAR
    ('d', 'a', 'y') -> Just DAY
    ('d', 'e', 'l') -> Just DEL
    ('d', 'e', 'n') -> Just DEN
    ('d', 'e', 'u') -> Just DEU 
    ('d', 'g', 'r') -> Just DGR
    ('d', 'i', 'n') -> Just DIN
    ('d', 'i', 'v') -> Just DIV
    ('d', 'o', 'i') -> Just DOI
    ('d', 'r', 'a') -> Just DRA
    ('d', 's', 'b') -> Just DSB
    ('d', 'u', 'a') -> Just DUA
    ('d', 'u', 'm') -> Just DUM
    ('d', 'u', 't') -> Just DUT
    ('d', 'y', 'u') -> Just DYU
    ('d', 'z', 'o') -> Just DZO
    ('e', 'f', 'i') -> Just EFI
    ('e', 'g', 'y') -> Just EGY
    ('e', 'k', 'a') -> Just EKA
    ('e', 'l', 'l') -> Just ELL 
    ('e', 'l', 'x') -> Just ELX
    ('e', 'n', 'g') -> Just ENG
    ('e', 'n', 'm') -> Just ENM
    ('e', 'p', 'o') -> Just EPO
    ('e', 's', 't') -> Just EST
    ('e', 'u', 's') -> Just EUS 
    ('e', 'w', 'e') -> Just EWE
    ('e', 'w', 'o') -> Just EWO
    ('f', 'a', 'n') -> Just FAN
    ('f', 'a', 'o') -> Just FAO
    ('f', 'a', 's') -> Just FAS 
    ('f', 'a', 't') -> Just FAT
    ('f', 'i', 'j') -> Just FIJ
    ('f', 'i', 'l') -> Just FIL
    ('f', 'i', 'n') -> Just FIN
    ('f', 'i', 'u') -> Just FIU
    ('f', 'o', 'n') -> Just FON
    ('f', 'r', 'a') -> Just FRA 
    ('f', 'r', 'e') -> Just FRE
    ('f', 'r', 'm') -> Just FRM
    ('f', 'r', 'o') -> Just FRO
    ('f', 'r', 'r') -> Just FRR
    ('f', 'r', 's') -> Just FRS
    ('f', 'r', 'y') -> Just FRY
    ('f', 'u', 'l') -> Just FUL
    ('f', 'u', 'r') -> Just FUR
    ('g', 'a', 'a') -> Just GAA
    ('g', 'a', 'y') -> Just GAY
    ('g', 'b', 'a') -> Just GBA
    ('g', 'e', 'm') -> Just GEM
    ('g', 'e', 'o') -> Just GEO
    ('g', 'e', 'r') -> Just GER
    ('g', 'e', 'z') -> Just GEZ
    ('g', 'i', 'l') -> Just GIL
    ('g', 'l', 'a') -> Just GLA
    ('g', 'l', 'e') -> Just GLE
    ('g', 'l', 'g') -> Just GLG
    ('g', 'l', 'v') -> Just GLV
    ('g', 'm', 'h') -> Just GMH
    ('g', 'o', 'h') -> Just GOH
    ('g', 'o', 'n') -> Just GON
    ('g', 'o', 'r') -> Just GOR
    ('g', 'o', 't') -> Just GOT
    ('g', 'r', 'b') -> Just GRB
    ('g', 'r', 'c') -> Just GRC
    ('g', 'r', 'e') -> Just GRE
    ('g', 'r', 'n') -> Just GRN
    ('g', 's', 'w') -> Just GSW
    ('g', 'u', 'j') -> Just GUJ
    ('g', 'w', 'i') -> Just GWI
    ('h', 'a', 'i') -> Just HAI
    ('h', 'a', 't') -> Just HAT
    ('h', 'a', 'u') -> Just HAU
    ('h', 'a', 'w') -> Just HAW
    ('h', 'e', 'b') -> Just HEB
    ('h', 'e', 'r') -> Just HER
    ('h', 'i', 'l') -> Just HIL
    ('h', 'i', 'm') -> Just HIM
    ('h', 'i', 'n') -> Just HIN
    ('h', 'i', 't') -> Just HIT
    ('h', 'm', 'n') -> Just HMN
    ('h', 'm', 'o') -> Just HMO
    ('h', 'r', 'v') -> Just HRV
    ('h', 's', 'b') -> Just HSB
    ('h', 'u', 'n') -> Just HUN
    ('h', 'u', 'p') -> Just HUP
    ('h', 'y', 'e') -> Just HYE 
    ('i', 'b', 'a') -> Just IBA
    ('i', 'b', 'o') -> Just IBO
    ('i', 'c', 'e') -> Just ICE
    ('i', 'd', 'o') -> Just IDO
    ('i', 'i', 'i') -> Just III
    ('i', 'j', 'o') -> Just IJO
    ('i', 'k', 'u') -> Just IKU
    ('i', 'l', 'e') -> Just ILE
    ('i', 'l', 'o') -> Just ILO
    ('i', 'n', 'a') -> Just INA
    ('i', 'n', 'c') -> Just INC
    ('i', 'n', 'd') -> Just IND
    ('i', 'n', 'e') -> Just INE
    ('i', 'n', 'h') -> Just INH
    ('i', 'p', 'k') -> Just IPK
    ('i', 'r', 'a') -> Just IRA
    ('i', 'r', 'o') -> Just IRO
    ('i', 's', 'l') -> Just ISL 
    ('i', 't', 'a') -> Just ITA
    ('j', 'a', 'v') -> Just JAV
    ('j', 'b', 'o') -> Just JBO
    ('j', 'p', 'n') -> Just JPN
    ('j', 'p', 'r') -> Just JPR
    ('j', 'r', 'b') -> Just JRB
    ('k', 'a', 'a') -> Just KAA
    ('k', 'a', 'b') -> Just KAB
    ('k', 'a', 'c') -> Just KAC
    ('k', 'a', 'l') -> Just KAL
    ('k', 'a', 'm') -> Just KAM
    ('k', 'a', 'n') -> Just KAN
    ('k', 'a', 'r') -> Just KAR
    ('k', 'a', 's') -> Just KAS
    ('k', 'a', 't') -> Just KAT 
    ('k', 'a', 'u') -> Just KAU
    ('k', 'a', 'w') -> Just KAW
    ('k', 'a', 'z') -> Just KAZ
    ('k', 'b', 'd') -> Just KBD
    ('k', 'h', 'a') -> Just KHA
    ('k', 'h', 'i') -> Just KHI
    ('k', 'h', 'm') -> Just KHM
    ('k', 'h', 'o') -> Just KHO
    ('k', 'i', 'k') -> Just KIK
    ('k', 'i', 'n') -> Just KIN
    ('k', 'i', 'r') -> Just KIR
    ('k', 'm', 'b') -> Just KMB
    ('k', 'o', 'k') -> Just KOK
    ('k', 'o', 'm') -> Just KOM
    ('k', 'o', 'n') -> Just KON
    ('k', 'o', 'r') -> Just KOR
    ('k', 'o', 's') -> Just KOS
    ('k', 'p', 'e') -> Just KPE
    ('k', 'r', 'c') -> Just KRC
    ('k', 'r', 'l') -> Just KRL
    ('k', 'r', 'o') -> Just KRO
    ('k', 'r', 'u') -> Just KRU
    ('k', 'u', 'a') -> Just KUA
    ('k', 'u', 'm') -> Just KUM
    ('k', 'u', 'r') -> Just KUR
    ('k', 'u', 't') -> Just KUT
    ('l', 'a', 'd') -> Just LAD
    ('l', 'a', 'h') -> Just LAH
    ('l', 'a', 'm') -> Just LAM
    ('l', 'a', 'o') -> Just LAO
    ('l', 'a', 't') -> Just LAT
    ('l', 'a', 'v') -> Just LAV
    ('l', 'e', 'z') -> Just LEZ
    ('l', 'i', 'm') -> Just LIM
    ('l', 'i', 'n') -> Just LIN
    ('l', 'i', 't') -> Just LIT
    ('l', 'o', 'l') -> Just LOL
    ('l', 'o', 'z') -> Just LOZ
    ('l', 't', 'z') -> Just LTZ
    ('l', 'u', 'a') -> Just LUA
    ('l', 'u', 'b') -> Just LUB
    ('l', 'u', 'g') -> Just LUG
    ('l', 'u', 'i') -> Just LUI
    ('l', 'u', 'n') -> Just LUN
    ('l', 'u', 'o') -> Just LUO
    ('l', 'u', 's') -> Just LUS
    ('m', 'a', 'c') -> Just MAC
    ('m', 'a', 'd') -> Just MAD
    ('m', 'a', 'g') -> Just MAG
    ('m', 'a', 'h') -> Just MAH
    ('m', 'a', 'i') -> Just MAI
    ('m', 'a', 'k') -> Just MAK
    ('m', 'a', 'l') -> Just MAL
    ('m', 'a', 'n') -> Just MAN
    ('m', 'a', 'o') -> Just MAO
    ('m', 'a', 'p') -> Just MAP
    ('m', 'a', 'r') -> Just MAR
    ('m', 'a', 's') -> Just MAS
    ('m', 'a', 'y') -> Just MAY
    ('m', 'd', 'f') -> Just MDF
    ('m', 'd', 'r') -> Just MDR
    ('m', 'e', 'n') -> Just MEN
    ('m', 'g', 'a') -> Just MGA
    ('m', 'i', 'c') -> Just MIC
    ('m', 'i', 'n') -> Just MIN
    ('m', 'k', 'd') -> Just MKD 
    ('m', 'k', 'h') -> Just MKH
    ('m', 'l', 'g') -> Just MLG
    ('m', 'l', 't') -> Just MLT
    ('m', 'n', 'c') -> Just MNC
    ('m', 'n', 'i') -> Just MNI
    ('m', 'n', 'o') -> Just MNO
    ('m', 'o', 'h') -> Just MOH
    ('m', 'o', 'n') -> Just MON
    ('m', 'o', 's') -> Just MOS
    ('m', 'r', 'i') -> Just MRI 
    ('m', 's', 'a') -> Just MSA 
    ('m', 'u', 'n') -> Just MUN
    ('m', 'u', 's') -> Just MUS
    ('m', 'w', 'l') -> Just MWL
    ('m', 'w', 'r') -> Just MWR
    ('m', 'y', 'a') -> Just MYA 
    ('m', 'y', 'n') -> Just MYN
    ('m', 'y', 'v') -> Just MYV
    ('n', 'a', 'h') -> Just NAH
    ('n', 'a', 'i') -> Just NAI
    ('n', 'a', 'p') -> Just NAP
    ('n', 'a', 'u') -> Just NAU
    ('n', 'a', 'v') -> Just NAV
    ('n', 'b', 'l') -> Just NBL
    ('n', 'd', 'e') -> Just NDE
    ('n', 'd', 'o') -> Just NDO
    ('n', 'd', 's') -> Just NDS
    ('n', 'e', 'p') -> Just NEP
    ('n', 'e', 'w') -> Just NEW
    ('n', 'i', 'a') -> Just NIA
    ('n', 'i', 'c') -> Just NIC
    ('n', 'i', 'u') -> Just NIU
    ('n', 'l', 'd') -> Just NLD 
    ('n', 'n', 'o') -> Just NNO
    ('n', 'o', 'b') -> Just NOB
    ('n', 'o', 'g') -> Just NOG
    ('n', 'o', 'n') -> Just NON
    ('n', 'o', 'r') -> Just NOR
    ('n', 'q', 'o') -> Just NQO
    ('n', 's', 'o') -> Just NSO
    ('n', 'u', 'b') -> Just NUB
    ('n', 'w', 'c') -> Just NWC
    ('n', 'y', 'a') -> Just NYA
    ('n', 'y', 'm') -> Just NYM
    ('n', 'y', 'n') -> Just NYN
    ('n', 'y', 'o') -> Just NYO
    ('n', 'z', 'i') -> Just NZI
    ('o', 'c', 'i') -> Just OCI
    ('o', 'j', 'i') -> Just OJI
    ('o', 'r', 'i') -> Just ORI
    ('o', 'r', 'm') -> Just ORM
    ('o', 's', 'a') -> Just OSA
    ('o', 's', 's') -> Just OSS
    ('o', 't', 'a') -> Just OTA
    ('o', 't', 'o') -> Just OTO
    ('p', 'a', 'a') -> Just PAA
    ('p', 'a', 'g') -> Just PAG
    ('p', 'a', 'l') -> Just PAL
    ('p', 'a', 'm') -> Just PAM
    ('p', 'a', 'n') -> Just PAN
    ('p', 'a', 'p') -> Just PAP
    ('p', 'a', 'u') -> Just PAU
    ('p', 'e', 'o') -> Just PEO
    ('p', 'e', 'r') -> Just PER
    ('p', 'h', 'i') -> Just PHI
    ('p', 'h', 'n') -> Just PHN
    ('p', 'l', 'i') -> Just PLI
    ('p', 'o', 'l') -> Just POL
    ('p', 'o', 'n') -> Just PON
    ('p', 'o', 'r') -> Just POR
    ('p', 'r', 'a') -> Just PRA
    ('p', 'r', 'o') -> Just PRO
    ('p', 'u', 's') -> Just PUS
    ('q', 'u', 'e') -> Just QUE
    ('r', 'a', 'j') -> Just RAJ
    ('r', 'a', 'p') -> Just RAP
    ('r', 'a', 'r') -> Just RAR
    ('r', 'o', 'a') -> Just ROA
    ('r', 'o', 'h') -> Just ROH
    ('r', 'o', 'm') -> Just ROM
    ('r', 'o', 'n') -> Just RON 
    ('r', 'u', 'm') -> Just RUM
    ('r', 'u', 'n') -> Just RUN
    ('r', 'u', 'p') -> Just RUP
    ('r', 'u', 's') -> Just RUS
    ('s', 'a', 'd') -> Just SAD
    ('s', 'a', 'g') -> Just SAG
    ('s', 'a', 'h') -> Just SAH
    ('s', 'a', 'i') -> Just SAI
    ('s', 'a', 'l') -> Just SAL
    ('s', 'a', 'm') -> Just SAM
    ('s', 'a', 'n') -> Just SAN
    ('s', 'a', 's') -> Just SAS
    ('s', 'a', 't') -> Just SAT
    ('s', 'c', 'n') -> Just SCN
    ('s', 'c', 'o') -> Just SCO
    ('s', 'e', 'l') -> Just SEL
    ('s', 'e', 'm') -> Just SEM
    ('s', 'g', 'a') -> Just SGA
    ('s', 'g', 'n') -> Just SGN
    ('s', 'h', 'n') -> Just SHN
    ('s', 'i', 'd') -> Just SID
    ('s', 'i', 'n') -> Just SIN
    ('s', 'i', 'o') -> Just SIO
    ('s', 'i', 't') -> Just SIT
    ('s', 'l', 'a') -> Just SLA
    ('s', 'l', 'o') -> Just SLO
    ('s', 'l', 'k') -> Just SLK 
    ('s', 'l', 'v') -> Just SLV
    ('s', 'm', 'a') -> Just SMA
    ('s', 'm', 'e') -> Just SME
    ('s', 'm', 'i') -> Just SMI
    ('s', 'm', 'j') -> Just SMJ
    ('s', 'm', 'n') -> Just SMN
    ('s', 'm', 'o') -> Just SMO
    ('s', 'm', 's') -> Just SMS
    ('s', 'n', 'a') -> Just SNA
    ('s', 'n', 'd') -> Just SND
    ('s', 'n', 'k') -> Just SNK
    ('s', 'o', 'g') -> Just SOG
    ('s', 'o', 'm') -> Just SOM
    ('s', 'o', 'n') -> Just SON
    ('s', 'o', 't') -> Just SOT
    ('s', 'p', 'a') -> Just SPA
    ('s', 'q', 'i') -> Just SQI 
    ('s', 'r', 'd') -> Just SRD
    ('s', 'r', 'n') -> Just SRN
    ('s', 'r', 'p') -> Just SRP
    ('s', 'r', 'r') -> Just SRR
    ('s', 's', 'a') -> Just SSA
    ('s', 's', 'w') -> Just SSW
    ('s', 'u', 'k') -> Just SUK
    ('s', 'u', 'n') -> Just SUN
    ('s', 'u', 's') -> Just SUS
    ('s', 'u', 'x') -> Just SUX
    ('s', 'w', 'a') -> Just SWA
    ('s', 'w', 'e') -> Just SWE
    ('s', 'y', 'c') -> Just SYC
    ('s', 'y', 'r') -> Just SYR
    ('t', 'a', 'h') -> Just TAH
    ('t', 'a', 'i') -> Just TAI
    ('t', 'a', 'm') -> Just TAM
    ('t', 'a', 't') -> Just TAT
    ('t', 'e', 'l') -> Just TEL
    ('t', 'e', 'm') -> Just TEM
    ('t', 'e', 'r') -> Just TER
    ('t', 'e', 't') -> Just TET
    ('t', 'g', 'k') -> Just TGK
    ('t', 'g', 'l') -> Just TGL
    ('t', 'h', 'a') -> Just THA
    ('t', 'i', 'b') -> Just TIB
    ('t', 'i', 'g') -> Just TIG
    ('t', 'i', 'r') -> Just TIR
    ('t', 'i', 'v') -> Just TIV
    ('t', 'k', 'l') -> Just TKL
    ('t', 'l', 'h') -> Just TLH
    ('t', 'l', 'i') -> Just TLI
    ('t', 'm', 'h') -> Just TMH
    ('t', 'o', 'g') -> Just TOG
    ('t', 'o', 'n') -> Just TON
    ('t', 'p', 'i') -> Just TPI
    ('t', 's', 'i') -> Just TSI
    ('t', 's', 'n') -> Just TSN
    ('t', 's', 'o') -> Just TSO
    ('t', 'u', 'k') -> Just TUK
    ('t', 'u', 'm') -> Just TUM
    ('t', 'u', 'p') -> Just TUP
    ('t', 'u', 'r') -> Just TUR
    ('t', 'u', 't') -> Just TUT
    ('t', 'v', 'l') -> Just TVL
    ('t', 'w', 'i') -> Just TWI
    ('t', 'y', 'v') -> Just TYV
    ('u', 'd', 'm') -> Just UDM
    ('u', 'g', 'a') -> Just UGA
    ('u', 'i', 'g') -> Just UIG
    ('u', 'k', 'r') -> Just UKR
    ('u', 'm', 'b') -> Just UMB
    ('u', 'r', 'd') -> Just URD
    ('u', 'z', 'b') -> Just UZB
    ('v', 'a', 'i') -> Just VAI
    ('v', 'e', 'n') -> Just VEN
    ('v', 'i', 'e') -> Just VIE
    ('v', 'o', 'l') -> Just VOL
    ('v', 'o', 't') -> Just VOT
    ('w', 'a', 'k') -> Just WAK
    ('w', 'a', 'l') -> Just WAL
    ('w', 'a', 'r') -> Just WAR
    ('w', 'a', 's') -> Just WAS
    ('w', 'e', 'l') -> Just WEL
    ('w', 'e', 'n') -> Just WEN
    ('w', 'l', 'n') -> Just WLN
    ('w', 'o', 'l') -> Just WOL
    ('x', 'a', 'l') -> Just XAL
    ('x', 'h', 'o') -> Just XHO
    ('y', 'a', 'o') -> Just YAO
    ('y', 'a', 'p') -> Just YAP
    ('y', 'i', 'd') -> Just YID
    ('y', 'o', 'r') -> Just YOR
    ('y', 'p', 'k') -> Just YPK
    ('z', 'a', 'p') -> Just ZAP
    ('z', 'b', 'l') -> Just ZBL
    ('z', 'e', 'n') -> Just ZEN
    ('z', 'g', 'h') -> Just ZGH
    ('z', 'h', 'a') -> Just ZHA
    ('z', 'h', 'o') -> Just ZHO
    ('z', 'n', 'd') -> Just ZND
    ('z', 'u', 'l') -> Just ZUL
    ('z', 'u', 'n') -> Just ZUN
    ('z', 'z', 'a') -> Just ZZA
    _ -> Nothing

language :: ISO639_2 -> String
language = head . languageList

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