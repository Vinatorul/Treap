program Treap;

uses
  Vcl.Forms,
  DebugUnit in 'DebugUnit.pas' {Form1},
  TreapUnit in 'TreapUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
