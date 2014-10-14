{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2000-09 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{**Time zones from http://www.worldtimezone.com/wtz-names/timezonenames.html }
unit itzones;

interface

type
  TZoneItem = record
    Abbrev: string;
    Descr: string;
    Region: string;
    Offset: Real;
  end;

  TArrayOfTimeZones = array of TZoneItem;

var
  TimeZonesArray: TArrayOfTimeZones;
{Meiname sto ECT}
const
  TimeZonesString =
  'Y,Y-Ray,Yankee,-12'#13#10+
  'BT,Bering Time,Asia,-11'#13#10+
  'BET,Bering Standard Time,Asia,-11'#13#10+
  'X,X-Ray,Military,-11'#13#10+
  'HST,Hawaii Standard Time,North America,-10'#13#10+
  'HAST,Hawaii-Aleutian Standard Time,North America,-10'#13#10+
  'CKT,Cook Islands Time,Pacific,-10'#13#10+
  'CAT,Central Alaskan Standard Time (until 1967),North America,-10'#13#10+
  'W,Wiskey,Military,-10'#13#10+
  'AKST,Alaska Standard Time,North America,-9'#13#10+
  'HNY,Heure Normale du Yukon,North America,-9'#13#10+
  'V,Victor,Military,-9'#13#10+
  'PST,Pacific Standard Time,North America,-8'#13#10+
  'HNP,Heure Normale du Pacifique,North America,-8'#13#10+
  'U,Uniform,Military,-8'#13#10+
  'HNR,Heure Normale des Rocheuses,North America,-7'#13#10+
  'MST,Mountain Standard Time,North America,-7'#13#10+
  'T,Tango,Military,-7'#13#10+
  'CST,Central Standard Time,North America,-6'#13#10+
  'EAST,Easter Island Time (Chile),South America,-6'#13#10+
  'HNC,Heure Normale du Centre,North America,-6'#13#10+
  'S,Sierra,Military,-6'#13#10+
  'EST,Eastern Standard Time,North America,-5'#13#10+
  'HNE,Heure Normale de l''Est,North America,-5'#13#10+
  'AST,Acre Standard Time (Brazil),South America,-5'#13#10+
  'COT,Colombia Time,South America,-5'#13#10+
  'R,Romeo,Military,-5'#13#10+
  'AST,Atlantic Standard Time,North America,-4'#13#10+
  'HNA,Heure Normale de l''Atlantique,North America,-4'#13#10+
  'CLT,Chile Time,South America,-4'#13#10+
  'BOT,Bolivia Time,South America,-4'#13#10+
  'Q,Quebec,Military,-4'#13#10+
  'NST,Newfoundland Standard Time,North America,-3.3'#13#10+
  'HNT,Heure Normale de Terre-Neuve,North America,-3.3'#13#10+
  'ART,Argentina Time,South America,-3'#13#10+
  'BRA,Brazil Time,South America,-3'#13#10+
  'BST,Brazil Time,South America,-3'#13#10+
  'EST,Eastern Brazil Standard Time,South America,-3'#13#10+
  'P,Papa,Military,-3'#13#10+
  'O,Oscar,Military,-2'#13#10+
  'AT,Azores time,Europe,-1'#13#10+
  'AZOT,Azores time,Europe,-1'#13#10+
  'CVT,Cape Verde Time,Africa,-1'#13#10+
  'N,November,Military,-1'#13#10+
  'WET,Western European Time,Europe,0'#13#10+
  'GMT,Greenwich Mean Time,Europe,0'#13#10+
  'UTC,Coordinated Universal Time,World,0'#13#10+
  'CUT,Coordinated Universal Time,World,0'#13#10+
  'TUC,Coordinated Universal Time,World,0'#13#10+
  'Z,Zulu,Military,0'#13#10+
  'CET,Central European Time,Europe,1'#13#10+
  'MEZ,Mitteleuropäische Zeit,Europe,1'#13#10+
  'DNT,Dansk Normal,Europe,1'#13#10+
  'A,Alpha,Military,1'#13#10+
  'EET,Eastern European Time,Europe,2'#13#10+
  'CAT,Central Africa Time,Africa,2'#13#10+
  'B,Beta,Military,2'#13#10+
  'MSK,Moscow Standard Time,Europe,3'#13#10+
  'EAT,East Africa Time,Africa,3'#13#10+
  'AST,Arabia Standard Time,Asia,3'#13#10+
  'C,Charlie,Military,3'#13#10+
  'AMT,Armenia Time,Asia,4'#13#10+
  'AZT,Azerbaijan Time,Asia,4'#13#10+
  'D,Delta,Military,4'#13#10+
  'AFT,Afghanistan Time,Asia,4.3'#13#10+
  'E,Echo,Military,5'#13#10+
  'BAT,Baghdad Time,Asia,6'#13#10+
  'BT,Baghdad Time,Asia,6'#13#10+
  'BTT,Bhutan Time,Asia,6'#13#10+
  'BDT,Bangladesh Time,Asia,6'#13#10+
  'F,Foxtrot,Military,6'#13#10+
  'CCT,Cocos Islands Time (Indian Ocean),Asia,6.3'#13#10+
  'CXT,Christmas Island Time,Indian Ocean,7'#13#10+
  'DAVT,Davis Time (Antarctica),Antarctica,7'#13#10+
  'G,Golf,Military,7'#13#10+
  'AWST,Australian Western Standard Time,Australia,8'#13#10+
  'WAST,Western Standard Time (Australia),Australia,8'#13#10+
  'WST,Western Standard Time (Australia),Australia,8'#13#10+
  'CCT,China Coast Time,Asia,8'#13#10+
  'CST,China Coast Time,Asia,8'#13#10+
  'BNT,Brunei Darussalam Time,Asia,8'#13#10+
  'BORT,Borneo Time (Indonesia),Asia,8'#13#10+
  'H,Hotel,Military,8'#13#10+
  'I,India,Military,9'#13#10+
  'ACST,Australian Central Standard Time,Australia,9.3'#13#10+
  'CST,Central Standard Time,Australia,9.3'#13#10+
  'AEST,Australian Eastern Standard Time,Australia,10'#13#10+
  'EST,Eastern Standard Time,Australia,10'#13#10+
  'ChST,Chamorro Standard Time (Guam),Other,10'#13#10+
  'GST,Guam Standard Time,Other,10'#13#10+
  'DDUT,Dumont-d`Urville Time (Antarctica),Antarctica,10'#13#10+
  'K,Kilo,Military,10'#13#10+
  'L,Lima,Military,11'#13#10+
  'NFT,Norfolk (Island) Time,Australia,11.3'#13#10+
  'M,Mike,Military,12'#13#10+
  'ANAT,Chukot Time (Russia),Asia,12'#13#10+
  'CHAST,Chatham Standard Time (New Zealand),New Zealand,1245'#13#10  ;


function RemoveTZDescr(AString: string): string;

function PickATZ(AOffsetMin: Integer): string;

implementation

uses istrUtils, Classes, SysUtils;

function AddPlusSign(AValue: Real): string;
begin
  Result := Format('%.4d', [Round(AValue*100)]);
  if AValue>=0 then
    Result := '+'+Result;
end;

function RemoveTZDescr(AString: string): string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  for i := 0 to Length(TimeZonesArray)-1 do
    with TimeZonesArray[i] do
    begin
      s := Abbrev+' (UTC'+ AddPlusSign(Offset) +')';
      if StrPos(PChar(AString), PChar(s))<>nil then
      begin
        Result := s;
        Break;
      end;
    end;
end;

function PickATZ(AOffsetMin: Integer): string;
var
  i, OffsetHours, OffsetMins: Integer;
  AOffsetAsFloat: Real;
begin
  Result := '';
  OffsetHours := AOffsetMin div 60;
  OffsetMins := AOffsetMin mod 60;
  AOffsetAsFloat := OffsetHours+OffsetMins/100;
  for i := 0 to Length(TimeZonesArray)-1 do
    with TimeZonesArray[i] do
      if Offset=AOffsetAsFloat then
      begin
        Result := Abbrev+' (UTC'+ AddPlusSign(Offset) +')';
        Break;
      end;
  if Result='' then
    Result := '(UTC'+ AddPlusSign(AOffsetAsFloat) +')';
end;

procedure InitializeTimeZonesArray;
var
  i: Integer;
  AStringList: TStringList;
  SavedDecimalSeparator: Char;
begin
  AStringList := nil;
  SavedDecimalSeparator := SysUtils.FormatSettings.DecimalSeparator;
  try
    SysUtils.FormatSettings.DecimalSeparator := '.';
    AStringList := TStringList.Create;
    AStringList.Text := TimeZonesString;
    SetLength(TimeZonesArray, AStringList.Count);
    for i := 0 to Length(TimeZonesArray)-1 do
      with TimeZonesArray[i] do
      begin
        Abbrev := DelimitedStringItem(AStringList[i], 1, ',');
        Descr := DelimitedStringItem(AStringList[i], 2, ',');
        Region := DelimitedStringItem(AStringList[i], 3, ',');
        Offset := StrToFloat(DelimitedStringItem(AStringList[i], 4, ','));
      end;
  finally
    AStringList.Free;
    SysUtils.FormatSettings.DecimalSeparator := SavedDecimalSeparator;
  end;
end;

initialization
  InitializeTimeZonesArray;

finalization
  TimeZonesArray := nil;

end.
