{This code has been copied from the Lazarus unit LazUTF8 and is distributed
 under them modified LGPL. See the file COPYING.modifiedLGPL.txt, included in the lAZARUS distribution,
  for details about the license.
}

function FindInvalidUTF8Codepoint(p: PAnsiChar; Count: PtrInt; StopOnNonUTF8: Boolean): PtrInt;
// return -1 if ok
var
  CharLen: Integer;
  c: Byte;
begin
  if (p<>nil) then begin
    Result:=0;
    while Result<Count do begin
      c:=ord(p^);
      if c<%10000000 then begin
        // regular single byte ASCII character (#0 is a character, this is Pascal ;)
        CharLen:=1;
      end else if c<=%11000001 then begin
        // single byte character, between valid UTF-8 encodings
        // %11000000 and %11000001 map 2 byte to #0..#128, which is invalid and used for XSS attacks
        if StopOnNonUTF8 or (c>=192) then
          exit;
        CharLen:=1;
      end else if c<=%11011111 then begin
        // could be 2 byte character (%110xxxxx %10xxxxxx)
        if (Result<Count-1)
        and ((ord(p[1]) and %11000000) = %10000000) then
          CharLen:=2
        else
          exit; // missing following bytes
      end
      else if c<=%11101111 then begin
        // could be 3 byte character (%1110xxxx %10xxxxxx %10xxxxxx)
        if (Result<Count-2)
        and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then begin
          if (c=%11100000) and (ord(p[1])<=%10011111) then
            exit; // XSS attack: 3 bytes are mapped to the 1 or 2 byte codes
          CharLen:=3;
        end else
          exit; // missing following bytes
      end
      else if c<=%11110111 then begin
        // could be 4 byte character (%11110xxx %10xxxxxx %10xxxxxx %10xxxxxx)
        if (Result<Count-3)
        and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then begin
          if (c=%11110000) and (ord(p[1])<=%10001111) then
            exit; // XSS attack: 4 bytes are mapped to the 1-3 byte codes
          if (c>%11110100) then
            exit; // out of range U+10FFFF
          if (c=%11110100) and (ord(p[1])>%10001111) then
            exit; // out of range U+10FFFF
          CharLen:=4;
        end else
          exit; // missing following bytes
      end
      else begin
        if StopOnNonUTF8 then
          exit;
        CharLen:=1;
      end;
      inc(Result,CharLen);
      inc(p,CharLen);
      if Result>Count then begin
        dec(Result,CharLen);
        exit; // missing following bytes
      end;
    end;
  end;
  // ok
  Result:=-1;
end;

