{******************************************************************}
{                                                                  }
{  Thelma library                                                  }
{                                                                  }
{  Copyright (c) 2001-10 National Technical University of Athens   }
{                                                                  }
{******************************************************************}

{** Utilities for files and directories. }
unit ifile;

interface

uses SysUtils, Classes;

{** Copies a file.
FileCopy is actually a wrapper for Win32 CopyFile function. Using CopyFile
returns an error value in case of an error, whereas FileCopy raises an
exception.

If the destination file already exists, it is overwritten.
@author A.X.
@SeeAlso <Jump File=Win32.hlp K=CopyFile Text=CopyFile>
}
procedure FileCopy(Source, Dest: string);

{** Deletes a directory and (recursively) all its contents.
RemoveTree deletes a directory with all its contents. The specified directory
may be an absolute or relative pathname. In case of an error it raises an
exception.

It is needless to say you should be very careful. Note also that
if RemoveTree is run on a Unix filesystem (say through samba sharing), it will
follow symbolic links.
@author A.X.
@SeeAlso <Jump File=Delphi5.hlp K="RmDir function" Text=RmDir>
@SeeAlso <Jump File=Delphi5.hlp K="RemoveDir function" Text=RemoveDir>
@SeeAlso <Jump File=Delphi5.hlp K="DeleteFile function" Text=DeleteFile>
}
procedure RemoveTree(Name: string);

{** Deletes an empty directory.
RemoveDirectory is the same as RmDir, except that when it raises an exception
the error message is really more decent than 'I/O error 32'.
@author A.X.
}
procedure RemoveDirectory(Name: string);

type
{** Reserved for future use with Grep function.}
  TGrepOption = (grpDummy);
{** Reserved for future use with Grep function.}
  TGrepOptions = set of TGrepOption;

{** Checks whether a file contains a specified string.
Grep returns True if the specified file contains the specified string.
In this version, Options must be empty.
@author A.X.
}
function Grep(FileName: string; s: string; Options: TGrepOptions):
  Boolean;

{** Appends a string to a file.
AppendToFile opens the specified for append, appends the specified string, and
then closes the file.
@author A.X.
}
procedure AppendToFile(FileName: string; s: string);

{** Find file, puts the result in FileList string list.
    It can do a recursive search if set Recursive to True.
    StartDir could be: 'C:\foufotos\'.
    File Mask, e.g. '*.txt' or '*.*'
    FileList is a string list to put result as new lines. FileList should
    be created before calling FindFiles, or else an AssertionFailure is
    raised.
    @author Stefanos
}
procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string;
  Recursive: Boolean); overload;

{** This is an overloaded version of FindFiles, with MaxDepth specified.
}
procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string;
  Recursive: Boolean; MaxDepth: Integer); overload;


implementation

uses Windows;

{$WARN UNSAFE_TYPE OFF}

procedure FileCopy(Source, Dest: string);
resourcestring
  rsCannotCopyFile = 'Cannot copy file';
  rsTo = 'to';
begin
  if not CopyFile(PChar(Source), PChar(Dest), False) then
    raise EInOutError.Create(rsCannotCopyFile+' '+Source+#13#10+rsTo+' '+Dest
      +':'#13#10+SysErrorMessage(GetLastError));
end;

{$WARN UNSAFE_TYPE ON}

procedure RemoveTree(Name: string);
resourcestring
  rsCannotDeleteFile = 'Cannot delete file';
var
  SearchRec: TSearchRec;
begin
  try
    if FindFirst(Name+'\*.*', faAnyFile, SearchRec)=0 then
      repeat
        if (SearchRec.Name='.') or (SearchRec.Name='..') then
          Continue;
        if (SearchRec.Attr and faDirectory)>0 then
          RemoveTree(Name+'\'+SearchRec.Name)
        else
          if not SysUtils.DeleteFile(Name+'\'+SearchRec.Name) then
            raise EInOutError.Create(rsCannotDeleteFile+' '+SearchRec.Name+': '
              +SysErrorMessage(GetLastError));
      until FindNext(SearchRec)>0;
  finally
    SysUtils.FindClose(SearchRec);
  end;
  RemoveDirectory(Name);
end;

procedure RemoveDirectory(Name: string);
resourcestring
  rsCannotDeleteDirectory = 'Cannot delete directory';
begin
  if not RemoveDir(Name) then
    raise EInOutError.Create(rsCannotDeleteDirectory+' '+Name+': '+
      SysErrorMessage(GetLastError));
end;

function Grep(FileName: string; s: string; Options: TGrepOptions):
  Boolean;
var
  F: TextFile;
  FileContents: string;
  c: Char;
begin
  Assign(F, FileName);
  Reset(F);
  try
    while not Eof(F) do
    begin
      Read(F, c);
      FileContents := FileContents+c;
    end;
  finally
    CloseFile(F);
  end;
  Result := Pos(s, FileContents)>0;
end;

procedure AppendToFile(FileName: string; s: string);
var
  F: TextFile;
begin
  Assign(F, FileName);
  Append(F);
  try
    Write(F, s);
  finally
    CloseFile(F);
  end;
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string;
  Recursive: Boolean);
begin
  FindFiles(FilesList, StartDir, FileMask, Recursive, 128);
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string;
  Recursive: Boolean; MaxDepth: Integer);
var
  i: Integer;
  ASearchRecord: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
begin
  Assert(FilesList<>nil);
  if StartDir[Length(StartDir)] <> '\' then
    StartDir := StartDir + '\';
  { Build a list of the files in directory StartDir
     (not the directories!)                         }
  IsFound := FindFirst(StartDir+FileMask, faAnyFile-faDirectory,
    ASearchRecord) = 0;
  while IsFound do begin
    FilesList.Add(StartDir + ASearchRecord.Name);
    IsFound := FindNext(ASearchRecord) = 0;
  end;
  {Using SysUtils workspace, as it takes Windows as default and bring
   compilation error! (Bliah!)}
  SysUtils.FindClose(ASearchRecord);
  if Recursive and (MaxDepth>0) then
  begin
    DirList := nil;
    try
      // Build a list of subdirectories
      DirList := TStringList.Create;
      IsFound := FindFirst(StartDir+'*.*', faAnyFile, ASearchRecord) = 0;
      while IsFound do begin
        if ((ASearchRecord.Attr and faDirectory) <> 0) and
             (ASearchRecord.Name[1] <> '.') then
          DirList.Add(StartDir + ASearchRecord.Name);
        IsFound := FindNext(ASearchRecord) = 0;
      end;
      SysUtils.FindClose(ASearchRecord);
      // Scan the list of subdirectories
      for i := 0 to DirList.Count - 1 do
        FindFiles(FilesList, DirList[i], FileMask, True, MaxDepth-1);
    finally
      DirList.Free;
    end;
  end;
end;

end.
