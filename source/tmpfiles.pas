{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2001 National Technical University of Athens      }
{                                                                  }
{******************************************************************}

{** Utilities for temporary file creation and automatic deletion. }
unit TmpFiles;

interface

{** Creates a temporary file name which will be automatically deleted.
CreateTempFileName creates a file name (with full pathname) with the specified
extension. The path is the system temporary directory, and it is guaranteed that
no such file exists. The function does not create the file. When the application
terminates any existing files with file names returned by this function are
automatically deleted.<p>
If you don't need any specific extension, use 'tmp'.
@author A.X.
}
function CreateTempFileName(FileExt: string): string;

implementation

{$WARN UNSAFE_TYPE OFF}  // DeleteFile API requires PChar

uses sysutils, classes, windows;

var
  TempFiles: TStringList;

function CreateTempFileName(FileExt: string): string;
var
  TempPath: array[0..127] of Char;
  i: Integer;
begin
  i := GetTempPath(127, TempPath);
  Assert((i>0) and (i<128));
  i := Round(Time*9999999);
  repeat
    Result := string(TempPath)+IntToStr(i)+'.'+FileExt;
    Inc(i);
  until not FileExists(Result);
  TempFiles.Add(Result);
end;

initialization
  TempFiles := TStringList.Create;

finalization
  if TempFiles<>nil then
    while TempFiles.Count>0 do
    begin
      DeleteFile(PChar(TempFiles[0]));
      TempFiles.Delete(0);
    end;
  TempFiles.Free;

end.
