grammar LiteralParser;

constant
    : numericLiteral
    | charLiteral
    | timeLiteral
    | bitStrLiteral
    | boolLiteral
    ;
numericLiteral
    : intLiteral
    | realLiteral
    ;
intLiteral
    : ( intTypeName '#' )? ( signedInt | binaryInt | octalInt | hexInt )
    ;
unsignedInt
    : digit ( '_'? digit )*
    ;
signedInt
    : ( '+' | '-' )? unsignedInt
    ;
binaryInt
    : '2#' ( '_'? bit )+
    ;
octalInt
    : '8#' ( '_'? octalDigit )+
    ;
hexInt
    : '16#' ( '_'? hexDigit )+
    ;
realLiteral
    : ( realTypeName '#' )? signedInt '.' unsignedInt ( E signedInt )?
    ;
bitStrLiteral
    : ( multibitsTypeName '#' )? ( unsignedInt | binaryInt | octalInt | hexInt )
    ;
boolLiteral
    : (boolTypeName '#' )? ( '0' | '1' | F A L S E | T R U E )
    ;

charLiteral
    : ( S T R I N G '#' )? charStr
    ;
charStr
    : sByteCharStr
    | dByteCharStr
    ;
sByteCharStr
    : '"' .* '"'
    ;
dByteCharStr
    : '\'' .* '\''
    ;

timeLiteral
    : duration
    | timeOfDay
    | date
    | dateAndTime
    ;
duration
    : ( timeTypeName | T | L T ) '#' ( '+' | '-' )? interval
    ;
fixPoint
    : unsignedInt ( '.' unsignedInt )?
    ;
interval
    : days
    | hours
    | minutes
    | seconds
    | milliseconds
    | microseconds
    | nanoseconds
    ;
days
    : fixPoint D
    | ( unsignedInt D '_'? )? hours?
    ;
hours
    : fixPoint H
    | ( unsignedInt H '_'? )? minutes?
    ;
minutes
    : fixPoint M
    | ( unsignedInt M '_'? )? seconds?
    ;
seconds
    : fixPoint S
    | ( unsignedInt S '_'? )? milliseconds?
    ;
milliseconds
    : fixPoint M S
    | ( unsignedInt M S '_'? )? microseconds?
    ;
microseconds
    : fixPoint U S
    | ( unsignedInt U S '_'? )? nanoseconds?
    ;
nanoseconds
    : fixPoint N S
    ;
timeOfDay
    : ( todTypeName | L T I M E '_' O F '_' D A Y ) '#' dayTime
    ;
dayTime
    : dayHour ':' dayMinute ':' daySecond
    ;
dayHour
    : unsignedInt
    ;
dayMinute
    : unsignedInt
    ;
daySecond
    : fixPoint
    ;
date
    : ( dateTypeName | D | L D ) '#' dateLiteral
    ;
dateLiteral
    : year '-' month '-' day
    ;
year
    : unsignedInt
    ;
month
    : unsignedInt
    ;
day
    : unsignedInt
    ;
dateAndTime
    : ( dtTypeName | L D A T E '_' A N D '_' T I M E ) '#' dateLiteral '-' dayTime
    ;

intTypeName
    : signedIntTypeName
    | unsignedIntTypeName
    ;
signedIntTypeName
    : S I N T
    | I N T
    | D I N T
    | L I N T
    ;
unsignedIntTypeName
    : U S I N T
    | U I N T
    | U D I N T
    | U L I N T
    ;
realTypeName
    : R E A L
    | L R E A L
    ;
timeTypeName
    : T I M E
    | L T I M E
    ;
dateTypeName
    : D A T E
    | L D A T E
    ;
todTypeName
    : T I M E '_' O F '_' D A Y
    | T O D
    | L T O D
    ;
dtTypeName
    : D A T E '_' A N D '_' T I M E
    | D T
    | L D T
    ;
boolTypeName
    : B O O L
    ;
multibitsTypeName
    : B Y T E
    | W O R D
    | D W O R D
    | L W O R D
    ;

digit
    : '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9'
    ;
bit
    : '0'
    | '1'
    ;
octalDigit
    : '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    ;
hexDigit
    : '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9'
    | A
    | B
    | C
    | D
    | E
    | F
    ;

A: [Aa];
B: [Bb];
C: [Cc];
D: [Dd];
E: [Ee];
F: [Ff];
G: [Gg];
H: [Hh];
I: [Ii];
J: [Jj];
K: [Kk];
L: [Ll];
M: [Mm];
N: [Nn];
O: [Oo];
P: [Pp];
Q: [Qq];
R: [Rr];
S: [Ss];
T: [Tt];
U: [Uu];
V: [Vv];
W: [Ww];
X: [Xx];
Y: [Yy];
Z: [Zz];
