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
var
  vLeft: TTreap<T>;
  vRight: TTreap<T>;
  vTempTreap: TTreap<T>;
begin
  Split(aPosition, vLeft, vRight);
  vTempTreap := TTreap<T>.Create(aData, Random(cRandCoof) + 1, nil, nil);
  vTempTreap := Merge(vLeft, vTempTreap);
  Self := Merge(vTempTreap, vRight);
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
var
  vLeft, vRight, vTemp: TTreap<T>;
begin
  Split(aPosition, vRight, vLeft);
  vRight.Split(1, vTemp, vRight);
  Self := Merge(vLeft, vRight);
end;

class function TTreap<T>.Merge(var aLeft, aRight: TTreap<T>): TTreap<T>;
var
  vRoot: TTreap<T>;
  vTempTreapP: ^TTreap<T>;
  vTempTreap: TTreap<T>;
  vStack: TStack<TTreap<T>>;
begin
  vTempTreapP := @vRoot;
  vStack := TStack<TTreap<T>>.Create;
  try
    while Assigned(aLeft) and Assigned(aRight) do
      if aLeft.FPriority > aRight.FPriority then
      begin
        vTempTreapP^ := aLeft;
        vStack.Push(aLeft);
        vTempTreapP := @aLeft.FRight;
        aLeft := vTempTreapP^;
      end
      else
      begin
        vTempTreapP^ := aRight;
        vStack.Push(aRight);
        vTempTreapP := @aRight.FLeft;
        aRight := vTempTreapP^;
      end;

    if Assigned(aLeft) then
      vTempTreapP^ := aLeft
    else
      vTempTreapP^ := aRight;
    Result := vRoot;
    while vStack.Count > 0 do
    begin
      vTempTreap := vStack.Pop;
      vTempTreap.ActualizeSize;
    end;
  finally
    vStack.Free;
  end;
end;

procedure TTreap<T>.Split(const aIndex: Integer; out aLeft, aRight: TTreap<T>);
var
  vNewTreap: TTreap<T>;
  vCurIndex: Integer;
begin
  vCurIndex := 0;
  if Assigned(FLeft) then
    vCurIndex := FLeft.FSize;

  if vCurIndex <= aIndex then
  begin
    if not Assigned(FRight) then
      aRight := nil
    else
      FRight.Split(aIndex - vCurIndex, vNewTreap, aRight);
    aLeft := TTreap<T>.Create(FData, FPriority, FLeft, vNewTreap);
    aLeft.ActualizeSize;
  end
  else
  begin
    if not Assigned(FLeft) then
      aLeft := nil
    else
      FLeft.Split(aIndex, aLeft, vNewTreap);
    aRight := TTreap<T>.Create(FData, FPriority, vNewTreap, FRight);
    aRight.ActualizeSize;
  end;
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
