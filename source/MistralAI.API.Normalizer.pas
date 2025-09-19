unit MistralAI.API.Normalizer;

interface

uses
  System.SysUtils, System.JSON;

type
  /// <summary>
  /// Provides functionality to normalize specified JSON string fields into a standardized array-of-objects format.
  /// </summary>
  TJSONNormalizer = record
  private
    class function DoNormalize(const Raw: string;
      const Path: TArray<string>): string; static;

    class function NormalizeNode(Node: TJSONValue;
      const Path: TArray<string>; Depth: Integer): Boolean; static;
  public
    /// <summary>
    /// Entry point to normalize a JSON string. If the path is empty or parsing fails, returns the original string.
    /// </summary>
    /// <param name="Raw">The raw JSON text to normalize.</param>
    /// <param name="Path">Sequence of keys and/or "*" wildcards indicating which fields to convert.</param>
    /// <returns>
    /// The normalized JSON string with target fields wrapped in the format:
    /// <c>[ { "type": "text", "text": "&lt;original&gt;" } ]</c>.
    /// Returns the original <paramref name="Raw"/> if no modifications were made or on parse error.
    /// </returns>
    class function Normalize(const Raw: string;
      const Path: TArray<string>): string; static;
  end;

implementation

{ TJSONNormalizer }

class function TJSONNormalizer.Normalize(const Raw: string;
  const Path: TArray<string>): string;
begin
  Result := DoNormalize(Raw, TArray<string>(Path));
end;

class function TJSONNormalizer.DoNormalize(const Raw: string;
  const Path: TArray<string>): string;
var
  Root: TJSONValue;
begin
  if Length(Path) = 0 then
    Exit(Raw);

  Root := TJSONObject.ParseJSONValue(Raw);
  if not Assigned(Root) then
    Exit(Raw);

  try
    if not NormalizeNode(Root, Path, 0) then
      Exit(Raw);

    if Root is TJSONObject then
      Result := TJSONObject(Root).ToJSON
    else
      Result := Root.ToJSON;
  finally
    Root.Free;
  end;
end;

class function TJSONNormalizer.NormalizeNode(Node: TJSONValue;
  const Path: TArray<string>; Depth: Integer): Boolean;
var
  Key     : string;
  NextVal : TJSONValue;
  JObj    : TJSONObject;
  JArr    : TJSONArray;
  S       : string;
  NewArr  : TJSONArray;
  OldPair : TJSONPair;
begin
  Result := False;

  if Depth >= Length(Path) then
    Exit;

  Key := Path[Depth];

  {--- wildcard "*" -> we browse the current table }
  if Key = '*' then
    begin
      if not (Node is TJSONArray) then
        Exit;

      JArr := TJSONArray(Node);
      for var Item in JArr do
        if NormalizeNode(Item, Path, Depth + 1) then
          Result := True;
      Exit;
    end;

  {--- explicit key -> we go down into the object }
  if not (Node is TJSONObject) then
    Exit;

  JObj := TJSONObject(Node);
  NextVal := JObj.GetValue(Key);
  if not Assigned(NextVal) then
    Exit;

  {--- Last link in the path: we are in the "happy" field }
  if Depth = High(Path) then
    begin
      if NextVal is TJSONArray then
        Exit;

      if NextVal is TJSONNull then
        Exit;

      if not (NextVal is TJSONString) then
        raise Exception.CreateFmt(
          'TJSONNormalizer: "%s" is neither JSONString nor JSONArray (got %s)',
          [Key, NextVal.ClassName]);

      S := TJSONString(NextVal).Value;

      NewArr := TJSONArray.Create;
      NewArr.Add(
        TJSONObject.Create
          .AddPair('type', 'text')
          .AddPair('text', S)
      );

      OldPair := JObj.RemovePair(Key);
      OldPair.Free;
      JObj.AddPair(Key, NewArr);
      Result := True;
      Exit;
    end;

  {--- Otherwise we continue to descend }
  Result := NormalizeNode(NextVal, Path, Depth + 1);
end;

end.
