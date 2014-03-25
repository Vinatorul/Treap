unit TreapUnit;

interface

uses Generics.Defaults;

type
  TTreap<T> = class;

  TTreapArray<T> = class
  private
    FComparer: TComparer<T>;

    FTreap: TTreap<T>;
  public
    constructor Create(const aComparer: TComparer<T>); overload;
    constructor Create(const aDataArray: TArray<T>;
      const aComparer: TComparer<T>); overload;
  end;

  TTreap<T> = class
  private
    FLeft: TTreap<T>;
    FRight: TTreap<T>;

    FKey: T;
    FPriority: Integer;

    FComparer: TComparer<T>;

    constructor Create(const aKey: T; const aPriority: Integer;
      const aLeft, aRight: TTreap<T>; const aComparer: TComparer<T>);
  public

    class function Merge(const aFirstTreap, aSecondTreap: TTreap<T>): TTreap<T>;
    procedure Split(const aKey: T; out aLeft, aRight: TTreap<T>);
  end;

implementation

{ TTreap<T> }

constructor TTreap<T>.Create(const aKey: T; const aPriority: Integer;
      const aLeft, aRight: TTreap<T>; const aComparer: TComparer<T>);
begin
  FLeft := aLeft;
  FRight := aRight;
  FKey := aKey;
  FPriority := aPriority;
  FComparer := aComparer;
end;

class function TTreap<T>.Merge(const aFirstTreap, aSecondTreap: TTreap<T>): TTreap<T>;
var
  vNewTreap: TTreap<T>;
begin
  Assert(Assigned(aFirstTreap) or Assigned(aSecondTreap));
  if not Assigned(aFirstTreap) then
    Exit(aSecondTreap);
  if not Assigned(aSecondTreap) then
    Exit(aFirstTreap);

  if aFirstTreap.FPriority > aSecondTreap.FPriority then
  begin
    vNewTreap := Merge(aFirstTreap.FRight, aSecondTreap);
    Result := TTreap<T>.Create(aFirstTreap.FKey, aFirstTreap.FPriority, 
      aFirstTreap.FLeft, vNewTreap, aFirstTreap.FComparer);
  end
  else
  begin
    vNewTreap := Merge(aFirstTreap, aSecondTreap.FLeft);
    Result := TTreap<T>.Create(aSecondTreap.FKey, aSecondTreap.FPriority, 
      vNewTreap, vNewTreap.FRight, aFirstTreap.FComparer);
  end;
end;

procedure TTreap<T>.Split(const aKey: T; out aLeft, aRight: TTreap<T>);
var
  vNewTreap: TTreap<T>;
begin
  if FComparer.Compare(FKey, aKey) <= 0 then
  begin
    if not Assigned(FRight) then
      aRight := nil
    else
      FRight.Split(aKey, vNewTreap, aRight);
    aLeft := TTreap<T>.Create(FKey, FPriority, FLeft, vNewTreap, FComparer);
  end
  else
  begin
    if not Assigned(FLeft) then
      aLeft := nil
    else
      FLeft.Split(aKey, aLeft, vNewTreap);
    aRight := TTreap<T>.Create(FKey, FPriority, vNewTreap, FRight, FComparer);
  end;
end;

{ TTreapArray<T> }

constructor TTreapArray<T>.Create(const aComparer: TComparer<T>);
begin
  FComparer := aComparer;
end;

constructor TTreapArray<T>.Create(const aDataArray: TArray<T>;
  const aComparer: TComparer<T>);
begin
  Assert(False);
  FComparer := aComparer;
end;

end.
