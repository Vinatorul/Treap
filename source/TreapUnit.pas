unit TreapUnit;

interface

uses Generics.Defaults, Generics.Collections, System.SysUtils;

type
  TTreap<T> = class;

  TTreapArray<T> = class
  private
    FTreap: TTreap<T>;
    FSize: Integer;
    FUpdStack: TObjectStack<TTreap<T>>;
    procedure ActualizeSizes;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Insert(const aPosition: Integer; const aData: T);
    procedure Remove(const aPosition: Integer);
    procedure Add(const aData: T);

    function High: Integer;
    function Count: Integer;

    class procedure Split(const aArray: TTreapArray<T>; const aInd: Integer;
      out aLeftPart, aRightPart: TTreapArray<T>);
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

    class function Merge(var aLeft, aRight: TTreap<T>;
      const aUpdStack: TObjectStack<TTreap<T>>): TTreap<T>;
    class procedure Split(const aIndex: Integer; const aTreap: TTreap<T>;
      out aLeft, aRight: TTreap<T>; const aUpdStack: TObjectStack<TTreap<T>>);

    procedure ActualizeSize;

    function FindByInd(const aInd: Integer): TTreap<T>;
  end;

const
  cRandCoef = 10000;

implementation

{ TTreap<T> }

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
  vInd := aInd;
  while Assigned(vTreap) do
  begin
    // ѕровер€ем количество элементов в левом поддереве.
    // ≈сли равно индексу, то корень - искомый узел
    // ≈сли больше, чем индекс, то будем искать в левом поддереве
    // ≈сли меньше, чем индекс, то будем искать в правом поддереве
    vLeftSize := 0;
    if Assigned(vTreap.FLeft) then
      vLeftSize := vTreap.FLeft.FSize + 1;
    if vLeftSize = vInd then
      Exit(vTreap);

    if vLeftSize > vInd then
      vTreap := vTreap.FLeft
    else
    begin
      vTreap := vTreap.FRight;
      Dec(vInd, vLeftSize);
    end;
  end;
  Result := nil;
end;

class function TTreap<T>.Merge(var aLeft, aRight: TTreap<T>;
  const aUpdStack: TObjectStack<TTreap<T>>): TTreap<T>;
var
  vRoot: TTreap<T>;
  vTempTreap: TTreap<T>;
  vIsLeft: Boolean;
begin
  vTempTreap := nil;
  vRoot := nil;
  // Ќа каждом шаге выбираетс€ вершина, у которой больше приоритет
  while Assigned(aLeft) and Assigned(aRight) do
    if aLeft.FPriority > aRight.FPriority then
    begin
      if not Assigned(vRoot) then  // перва€ итераци€?
        vRoot := aLeft
      else if not vIsLeft then    // мен€лось ли дерево
        vTempTreap.FLeft := aLeft;  // "перецепл€ем" ветку
      aUpdStack.Push(aLeft);       // запоминаем дл€ актуализации
      vTempTreap := aLeft;
      aLeft := aLeft.FRight;   // двигаемс€ в правое поддерево левого дерева (это нужно дл€ "безболезненной" склейки массивов)
      vIsLeft := True;           // сигнализируем, что мы были в левом дереве
    end
    else
    begin
      if not Assigned(vRoot) then   // перва€ итераци€?
        vRoot := aRight
      else if vIsLeft then    // мен€лось ли дерево
        vTempTreap.FRight := aRight;  // "перецепл€ем" ветку
      aUpdStack.Push(aRight);       // запоминаем дл€ актуализации
      vTempTreap := aRight;
      aRight := aRight.FLeft; // двигаемс€ в левое поддерево правого дерева (это нужно дл€ "безболезненной" склейки массивов)
      vIsLeft := False;         // сигнализируем, что мы были в правом дереве
    end;
  if Assigned(aLeft) then
  begin
    if not Assigned(vRoot) then   // перва€ итераци€?
      vRoot := aLeft
    else if not vIsLeft then
      vTempTreap.FLeft := aLeft  // правое дерево кончилось => полностью "перецепл€ем" левое
  end
  else
  begin
    if not Assigned(vRoot) then   // перва€ итераци€?
      vRoot := aRight
    else if vIsLeft then
      vTempTreap.FRight := aRight  // левое дерево кончилось => полностью "перецепл€ем" правое
  end;
  Result := vRoot;
end;

class procedure TTreap<T>.Split(const aIndex: Integer; const aTreap: TTreap<T>;
  out aLeft, aRight: TTreap<T>; const aUpdStack: TObjectStack<TTreap<T>>);
var
  vCurIndex: Integer;
  vTreap: TTreap<T>;
  vTempLeft: TTreap<T>;
  vTempRight: TTreap<T>;
  vInd: Integer;
begin
  if not Assigned(aTreap) then
    Exit;
  vTreap := aTreap;
  aLeft := nil;
  aRight := nil;
  vTempLeft := nil;
  vTempRight := nil;
  vInd := aIndex;
  while Assigned(vTreap) do
  begin
    vCurIndex := 0;
    if Assigned(vTreap.FLeft) then
      vCurIndex := vTreap.FLeft.FSize;
    if vCurIndex >= vInd then
    // ≈сли искомый индекс меньше количества элементов левого поддерева, то будем искать в нЄм
    begin
      if Assigned(aRight) then
      begin
        vTempRight.FLeft := vTreap;
        vTempRight := vTempRight.FLeft;
        aUpdStack.Push(vTempRight);
      end
      else
      begin
        aRight := vTreap;
        vTempRight := aRight;
      end;
      vTreap := vTreap.FLeft;
    end
    else if vCurIndex < vInd then
    // ≈сли искомый индекс больше количества элементов левого поддерева, то будем искать в правом поддереве
    begin
      if Assigned(aLeft) then
      begin
        vTempLeft.FRight := vTreap;
        vTempLeft := vTempLeft.FRight;
        aUpdStack.Push(vTempRight);
      end
      else
      begin
        aLeft := vTreap;
        vTempLeft := aLeft;
      end;
      vTreap := vTreap.FRight;
      Dec(vInd, vCurIndex + 1);
    end;
  end;
  if Assigned(vTempLeft) then
  begin
    vTempLeft.FRight := nil;
    aUpdStack.Push(vTempLeft);
  end;
  if Assigned(vTempRight) then
  begin
    vTempRight.FLeft := nil;
    aUpdStack.Push(vTempRight);
  end;
end;

{ TTreapArray<T> }

procedure TTreapArray<T>.ActualizeSizes;
var
  vTempTreap: TTreap<T>;
begin
  while FUpdStack.Count > 0 do  // актуализаци€
    begin
      vTempTreap := FUpdStack.Peek;
      if Assigned(vTempTreap) then
        vTempTreap.ActualizeSize;
      FUpdStack.Pop;
    end;
end;

procedure TTreapArray<T>.Add(const aData: T);
begin
  Insert(FSize, aData);
end;

function TTreapArray<T>.Count: Integer;
begin
  Result := FSize;
end;

constructor TTreapArray<T>.Create;
begin
  FTreap := nil;
  FUpdStack := TObjectStack<TTreap<T>>.Create(False);
  FSize := 0;
end;

destructor TTreapArray<T>.Destroy;
begin
  if Assigned(FTreap) then  // ToDo: доделать цепное удаление
    FTreap.Free;
  if Assigned(FUpdStack) then
    FUpdStack.Free;
  inherited;
end;

function TTreapArray<T>.High: Integer;
begin
  Result := FSize - 1;
end;

procedure TTreapArray<T>.Insert(const aPosition: Integer; const aData: T);
var
  vNewTreap: TTreap<T>;
  vLeftPart: TTreap<T>;
  vRightPart: TTreap<T>;
  vTempTreap: TTreap<T>;
begin
  if not Assigned(FTreap) then
    FTreap := TTreap<T>.Create(aData, Random(cRandCoef), nil, nil)
  else
  begin
    TTreap<T>.Split(aPosition, FTreap, vLeftPart, vRightPart, FUpdStack);
    vNewTreap := TTreap<T>.Create(aData, Random(cRandCoef), nil, nil);
    try
      vTempTreap := TTreap<T>.Merge(vLeftPart, vNewTreap, FUpdStack);
      FTreap := TTreap<T>.Merge(vTempTreap, vRightPart, FUpdStack);
      Inc(FSize);
      FUpdStack.Push(FTreap);
    except
      vNewTreap.Free;
    end;
  end;
  ActualizeSizes;
end;

procedure TTreapArray<T>.Remove(const aPosition: Integer);
var
  vTempTreap: TTreap<T>;
  vTempLeft, vTempRight: TTreap<T>;
  vMid: TTreap<T>;
  vNewRight: TTreap<T>;
begin
  if not Assigned(FTreap) then
    raise ERangeError.Create('Ёлемента с номером ' + IntToStr(aPosition) + ' не существует');
  TTreap<T>.Split(aPosition, FTreap, vTempLeft, vTempRight, FUpdStack);
  ActualizeSizes;
  if not Assigned(vTempRight) then
    raise ERangeError.Create('Ёлемента с номером ' + IntToStr(aPosition) + ' не существует');
  TTreap<T>.Split(1, vTempRight, vMid, vNewRight, FUpdStack);
  FTreap := TTreap<T>.Merge(vTempLeft, vNewRight, FUpdStack);
  Dec(FSize);
  vMid.Free;
  ActualizeSizes;
end;

class procedure TTreapArray<T>.Split(const aArray: TTreapArray<T>;
  const aInd: Integer; out aLeftPart, aRightPart: TTreapArray<T>);
begin

end;

end.
