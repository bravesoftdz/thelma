program setup;

uses
  Forms,
  main in 'main.pas' {FrmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
