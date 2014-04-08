unit TreapUnit;

interface

uses Generics.Defaults, Generics.Collections;

type
  TTreap<T> = class;

  TTreapArray<T> = class
  private
    FTreap: TTreap<T>;
    FUpdStack: TObjectStack<TTreap<T>>;
    procedure ActualizeSizes;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTreap<T> = class
  strict private
    FLeft: TTreap<T>;
    FRight: TTreap<T>;
    FData: T;
    FSize: Integer;
    FPriority: Integer;
  public
    constructor Create(const aData: T; const aPriority: Integer;
      const aLeft: TTreap<T>; const aRight: TTreap<T>);

    class function Merge(var aLeft, aRight: TTreap<T>; var aUpdStack: TObjectStack<TTreap<T>>): TTreap<T>;

    procedure ActualizeSize;
    procedure Split(const aIndex: Integer; out aLeft, aRight: TTreap<T>);
    procedure Insert(const aPosition: Integer; const aData: T);
    procedure Remove(const aPosition: Integer; const aData: T);
    function FindByInd(const aInd: Integer): TTreap<T>;
  end;

const
  cRandCoof = 1000;

implementation

{ TTreap<T> }

procedure TTreap<T>.Insert(const aPosition: Integer; const aData: T);
begin
end;

procedure TTreap<T>.ActualizeSize;
begin
  FSize := 1;
  if Assigned(FLeft) then
    Inc(FSize, FLeft.FSize);
  if Assigned(FRight) then
    Inc(FSize, FRight.FSize);
end;

constructor TTreap<T>.Create(const aData: T; const aPriority: Integer;
      const aLeft, aRight: TTreap<T>);
begin
  FLeft := aLeft;
  FRight := aRight;
  FData := aData;
  FPriority := aPriority;
  ActualizeSize;
end;

function TTreap<T>.FindByInd(const aInd: Integer): TTreap<T>;
var
  vTreap: TTreap<T>;
  vLeftSize: Integer;
  vInd: Integer;
begin
  vTreap := Self;
  vInd := aInd ;
  while Assigned(vTreap) do
  begin
    // ��������� ���������� ��������� � ����� ���������. ���� ����� �������, �� ������ - ������� ����
    // ���� ������, ��� ������, �� ����� ������ � ����� ���������
    // ���� ������, ��� ������, �� ����� ������ � ������ ���������
    vLeftSize := 0;
    if Assigned(vTreap.FLeft) then
      vLeftSize := vTreap.FLeft.FSize;
    if vLeftSize = vInd then
      Exit(vTreap);

    if vLeftSize > vInd then
      vTreap := vTreap.FLeft
    else
    begin
      vTreap := vTreap.FRight;
      Dec(vInd, vLeftSize + 1);
    end;
  end;
  Result := nil;
end;

procedure TTreap<T>.Remove(const aPosition: Integer; const aData: T);
begin

end;

class function TTreap<T>.Merge(var aLeft, aRight: TTreap<T>; var aUpdStack: TObjectStack<TTreap<T>>): TTreap<T>;
var
  vRoot: TTreap<T>;
  vTempTreap: TTreap<T>;
  vIsLeft: Boolean;
begin
  vTempTreap := nil;
  vRoot := nil;
  // �� ������ ���� ���������� �������, � ������� ������ ���������
  while Assigned(aLeft) and Assigned(aRight) do
    if aLeft.FPriority > aRight.FPriority then
    begin
      if not Assigned(vRoot) then  // ������ ��������?
        vRoot := aLeft
      else if not vIsLeft then    // �������� �� ������
        vTempTreap.FLeft := aLeft;  // "�����������" �����
      aUpdStack.Push(aLeft);       // ���������� ��� ������������
      vTempTreap := aLeft;
      aLeft := aLeft.FRight;   // ��������� � ������ ��������� ������ ������ (��� ����� ��� "��������������" ������� ��������)
      vIsLeft := True;           // �������������, ��� �� ���� � ����� ������
    end
    else
    begin
      if not Assigned(vRoot) then   // ������ ��������?
        vRoot := aRight
      else if vIsLeft then    // �������� �� ������
        vTempTreap.FRight := aRight;  // "�����������" �����
      aUpdStack.Push(aRight);       // ���������� ��� ������������
      vTempTreap := aRight;
      aRight := aRight.FLeft; // ��������� � ����� ��������� ������� ������ (��� ����� ��� "��������������" ������� ��������)
      vIsLeft := False;         // �������������, ��� �� ���� � ������ ������
    end;
  if Assigned(aLeft) then
  begin
    if not Assigned(vRoot) then   // ������ ��������?
      vRoot := aLeft
    else if not vIsLeft then
      vTempTreap.FLeft := aLeft  // ������ ������ ��������� => ��������� "�����������" �����
  end
  else
  begin
    if not Assigned(vRoot) then   // ������ ��������?
      vRoot := aRight
    else if vIsLeft then
      vTempTreap.FRight := aRight  // ����� ������ ��������� => ��������� "�����������" ������
  end;
  Result := vRoot;
end;

procedure TTreap<T>.Split(const aIndex: Integer; out aLeft, aRight: TTreap<T>);
begin

end;

{ TTreapArray<T> }

procedure TTreapArray<T>.ActualizeSizes;
begin
  while FUpdStack.Count > 0 do  // ������������
    begin
      FUpdStack.Peek.ActualizeSize;
      FUpdStack.Pop;
    end;
end;

constructor TTreapArray<T>.Create;
begin
  FTreap := nil;
  FUpdStack := TObjectStack<TTreap<T>>.Create(False);
end;

destructor TTreapArray<T>.Destroy;
begin
  if Assigned(FTreap) then
    FTreap.Free;
  if Assigned(FUpdStack) then
    FUpdStack.Free;
  inherited;
end;

end.
