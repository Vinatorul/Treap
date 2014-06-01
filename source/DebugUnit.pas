unit DebugUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, TreapUnit, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  vTreapArr: TTreapArray<Integer>;
begin
  vTreapArr := TTreapArray<Integer>.Create;
  vTreapArr.Insert(0, 0);
  vTreapArr.Insert(1, 1);
  vTreapArr.Insert(1, 2);
  vTreapArr.Remove(1);
end;

end.
