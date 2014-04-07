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
  vLeftTreap, vRightTreap: TTreap<Integer>;
begin
  vLeftTreap := TTreap<Integer>.Create(3, 4, nil, nil);
  vRightTreap := TTreap<Integer>.Create(1, 2, nil, nil);
  vLeftTreap := TTreap<Integer>.Merge(vLeftTreap,  vRightTreap);
  vRightTreap := TTreap<Integer>.Create(5, 1, nil, nil);
  vLeftTreap := TTreap<Integer>.Merge(vLeftTreap, vRightTreap);
  vRightTreap := vLeftTreap.FindByInd(2);
end;

end.
