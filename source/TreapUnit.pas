unit TreapUnit;

interface

uses Generics.Defaults, Generics.Collections;

type
  TTreap<T> = class;

  TTreapArray<T> = class
  private
    FTreap: TTreap<T>;
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

    procedure ActualizeSize;
  public
    constructor Create(const aData: T; const aPriority: Integer;
      const aLeft: TTreap<T>; const aRight: TTreap<T>);

    class function Merge(var aLeft, aRight: TTreap<T>): TTreap<T>;
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

class function TTreap<T>.Merge(var aLeft, aRight: TTreap<T>): TTreap<T>;
const
  cLeft = -1;   // ��������� � ����� ������
  cRight = 1;   // ��������� � ������ ������
var
  vRoot: TTreap<T>;
  vTempTreap: TTreap<T>;
  vUpdStack: TObjectStack<TTreap<T>>;
  vWay: Integer;
begin
  vTempTreap := nil;
  vRoot := nil;
  vUpdStack := TObjectStack<TTreap<T>>.Create(False);// ���� ����� �� ������������
  try
    // �� ������ ���� ���������� �������, � ������� ������ ���������
    while Assigned(aLeft) and Assigned(aRight) do
      if aLeft.FPriority > aRight.FPriority then
      begin
        if not Assigned(vRoot) then  // ������ ��������?
          vRoot := aLeft
        else if vWay = cRight then    // �������� �� ������
          vTempTreap.FLeft := aLeft;  // "�����������" �����
        vUpdStack.Push(aLeft);       // ���������� ��� ������������
        vTempTreap := aLeft;
        aLeft := aLeft.FRight;   // ��������� � ������ ��������� ������ ������ (��� ����� ��� "��������������" ������� ��������)
        vWay := cLeft;           // �������������, ��� �� ���� � ����� ������
      end
      else
      begin
        if not Assigned(vRoot) then   // ������ ��������?
          vRoot := aRight
        else if vWay = cLeft then    // �������� �� ������
          vTempTreap.FRight := aRight;  // "�����������" �����
        vUpdStack.Push(aRight);       // ���������� ��� ������������
        vTempTreap := aRight;
        aRight := aRight.FLeft; // ��������� � ����� ��������� ������� ������ (��� ����� ��� "��������������" ������� ��������)
        vWay := cRight;         // �������������, ��� �� ���� � ������ ������
      end;
    if Assigned(aLeft) then
    begin
      if not Assigned(vRoot) then   // ������ ��������?
        vRoot := aLeft
      else if vWay = cRight then
        vTempTreap.FLeft := aLeft  // ������ ������ ��������� => ��������� "�����������" �����

    end
    else
    begin
      if not Assigned(vRoot) then   // ������ ��������?
        vRoot := aRight
      else if vWay = cLeft then
        vTempTreap.FRight := aRight  // ����� ������ ��������� => ��������� "�����������" ������
    end;
    Result := vRoot;
    while vUpdStack.Count > 0 do  // ������������
    begin
      vTempTreap := vUpdStack.Peek;
      vUpdStack.Pop;
      vTempTreap.ActualizeSize;
    end;
  finally
    vUpdStack.Free;
  end;
end;

procedure TTreap<T>.Split(const aIndex: Integer; out aLeft, aRight: TTreap<T>);
begin

end;

{ TTreapArray<T> }

constructor TTreapArray<T>.Create;
begin
  FTreap := nil;
end;

destructor TTreapArray<T>.Destroy;
begin
  if Assigned(FTreap) then
    FTreap.Free;
  inherited;
end;

end.
