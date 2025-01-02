unit MistralAI.NetEncoding.Base64;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding, System.Net.Mime;

  /// <summary>
  /// Retrieves the MIME type of the specified file based on its location.
  /// </summary>
  /// <param name="FileLocation">The full path to the file whose MIME type is to be resolved.</param>
  /// <returns>
  /// A string representing the MIME type of the file.
  /// If the file does not exist, an exception will be raised.
  /// </returns>
  /// <exception cref="Exception">
  /// Thrown if the specified file cannot be found at the provided location.
  /// </exception>
  /// <remarks>
  /// This method checks if the specified file exists and retrieves its MIME type
  /// using the <c>TMimeTypes.Default.GetFileInfo</c> method.
  /// Ensure the provided path is valid before calling this function.
  /// </remarks>
  function ResolveMimeType(const FileLocation: string): string;

  /// <summary>
  /// Validates a URL or file location and converts it into a Base64 data URI if appropriate.
  /// </summary>
  /// <param name="Value">
  /// The input string, which can either be a URL or a file path.
  /// </param>
  /// <returns>
  /// A string representing the validated URL or a Base64-encoded data URI for supported image files.
  /// </returns>
  /// <exception cref="Exception">
  /// Thrown in the following cases:
  /// <para>If the URL starts with "http:" (insecure connection).</para>
  /// <para>If the file MIME type is not among the supported image formats: "image/png", "image/jpeg", "image/gif", "image/webp".</para>
  /// <para>If the file does not exist at the specified location.</para>
  /// </exception>
  /// <remarks>
  /// This function performs the following actions:
  /// <para>Checks if the input starts with "https:" and returns it directly if valid.</para>
  /// <para>Throws an exception if the input starts with "http:" to enforce secure connections.</para>
  /// <para>If the input is a file path, resolves its MIME type and verifies if it is a supported image format.</para>
  /// <para>Encodes the file content as a Base64 data URI if it is a supported image.</para>
  /// </remarks>
  function UrlCheck(const Value: string): string;

  /// <summary>
  /// Converts a byte array into a Base64-encoded string.
  /// </summary>
  /// <param name="Value">
  /// A <c>TBytes</c> array containing the binary data to be encoded.
  /// </param>
  /// <returns>
  /// A <c>string</c> representing the Base64-encoded content of the byte array.
  /// </returns>
  /// <exception cref="Exception">
  /// Raised if the provided byte array is empty.
  /// </exception>
  /// <remarks>
  /// This function processes the provided byte array, converts it into a memory stream, and encodes the content as a Base64 string.
  /// Ensure the byte array contains data before calling this function.
  /// </remarks>
  function BytesToBase64(const Value: TBytes): string;

  /// <summary>
  /// Converts a byte array into a UTF-8 encoded string.
  /// </summary>
  /// <param name="Value">
  /// A <c>TBytes</c> array containing the binary data to be converted.
  /// </param>
  /// <returns>
  /// A <c>string</c> representing the UTF-8 encoded content of the byte array.
  /// </returns>
  /// <exception cref="Exception">
  /// Raised if the provided byte array is empty.
  /// </exception>
  /// <remarks>
  /// This function processes the provided byte array by writing it to a memory stream,
  /// then reads and converts the data into a string using UTF-8 encoding.
  /// Ensure the byte array contains data before calling this function.
  /// </remarks>
  function BytesToString(const Value: TBytes): string;

  /// <summary>
  /// Encodes the content of a file into a Base64-encoded string.
  /// </summary>
  /// <param name="FileLocation">The full path to the file that will be encoded.</param>
  /// <returns>A Base64-encoded string representing the content of the file.</returns>
  /// <exception cref="Exception">Thrown if the specified file does not exist at the provided location.</exception>
  /// <remarks>
  /// This method reads the file from the specified location and converts it to a Base64 string.
  /// It uses different encoding methods depending on the version of  the RTL.
  /// For RTL version 35.0 and later, it uses <c>TNetEncoding.Base64String.Encode</c>,
  /// and for earlier versions, it uses <c>TNetEncoding.Base64.Encode</c>.
  /// </remarks>
  function EncodeBase64(FileLocation : string) : WideString; overload;

  /// <summary>
  /// Encodes the content of a stream into a Base64-encoded string.
  /// </summary>
  /// <param name="Value">
  /// A <c>TStream</c> containing the data to be encoded.
  /// </param>
  /// <returns>
  /// A <c>WideString</c> representing the Base64-encoded content of the stream.
  /// </returns>
  /// <exception cref="Exception">
  /// Raised if an error occurs while reading from the stream or during encoding.
  /// </exception>
  /// <remarks>
  /// This function reads the content of the provided stream and converts it into a Base64-encoded string.
  /// Ensure that the stream is properly positioned and contains readable data before calling this function.
  /// For RTL version 35.0 and later, it uses <c>TNetEncoding.Base64String.Encode</c>.
  /// For earlier versions, it uses <c>TNetEncoding.Base64.Encode</c>.
  /// </remarks>
  function EncodeBase64(const Value: TStream): WideString; overload;

  /// <summary>
  /// Decodes a Base64-encoded string and writes the resulting binary data to a specified file.
  /// </summary>
  /// <param name="Base64Str">The Base64-encoded string to decode.</param>
  /// <param name="FileName">The full path and name of the file where the decoded data will be written.</param>
  /// <exception cref="Exception">
  /// Thrown if the Base64 string cannot be decoded or if there is an error writing to the specified file.
  /// </exception>
  procedure DecodeBase64ToFile(const Base64Str: string; const FileName: string);

  /// <summary>
  /// Decodes a Base64-encoded string and writes the resulting binary data to the provided stream.
  /// </summary>
  /// <param name="Base64Str">The Base64-encoded string to decode.</param>
  /// <param name="Stream">The stream where the decoded binary data will be written. The stream should be writable.</param>
  /// <exception cref="Exception">
  /// Thrown if the Base64 string cannot be decoded or if there is an error writing to the provided stream.
  /// </exception>
  /// <remarks>
  /// After decoding, the stream's position is reset to the beginning.
  /// Ensure that the stream is properly managed and freed after use to avoid memory leaks.
  /// </remarks>
  procedure DecodeBase64ToStream(const Base64Str: string; const Stream: TStream);

implementation

uses
  System.StrUtils;

function EncodeBase64(FileLocation : string): WideString;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found : %s', [FileLocation]);

  var Stream := TMemoryStream.Create;
  var StreamOutput := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(FileLocation);
    Stream.Position := 0;
    {$IF RTLVersion >= 35.0}
    TNetEncoding.Base64String.Encode(Stream, StreamOutput);
    {$ELSE}
    TNetEncoding.Base64.Encode(Stream, StreamOutput);
    {$ENDIF}
    Result := StreamOutput.DataString;
  finally
    Stream.Free;
    StreamOutput.Free;
  end;
end;

function ResolveMimeType(const FileLocation: string): string;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found: %s', [FileLocation]);

  var LKind: TMimeTypes.TKind;
  TMimeTypes.Default.GetFileInfo(FileLocation, Result, LKind);
end;

function UrlCheck(const Value: string): string;
begin
  if Value.StartsWith('https:') then
    Exit(Value);
  if Value.StartsWith('http:') then
    raise Exception.Create('Invalid URL: Secure HTTPS connection required');

  var MimeType := ResolveMimeType(Value);
  if IndexStr(MimeType, ['image/png', 'image/jpeg', 'image/gif', 'image/webp']) = -1 then
    raise Exception.Create('Unsupported image format');
  Result :=  Format('data:%s;base64,%s', [MimeType, EncodeBase64(Value)]);
end;

function BytesToBase64(const Value: TBytes): String;
begin
  if Length(Value) = 0 then
    raise Exception.Create('No data recieved.');
  var MemStream := TMemoryStream.Create;
  try
    MemStream.WriteBuffer(Value[0], Length(Value));
    MemStream.Position := 0;
    Result := EncodeBase64(MemStream);
  finally
    MemStream.Free;
  end;
end;

function BytesToString(const Value: TBytes): string;
begin
  if Length(Value) = 0 then
    raise Exception.Create('No data recieved.');
  var MemStream := TMemoryStream.Create;
  try
    MemStream.WriteBuffer(Value[0], Length(Value));
    MemStream.Position := 0;
    var Reader := TStreamReader.Create(MemStream, TEncoding.UTF8);
    try
      Result := Reader.ReadToEnd;
    finally
      Reader.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

function EncodeBase64(const Value: TStream): WideString; overload;
begin
  var Stream := TMemoryStream.Create;
  var StreamOutput := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromStream(Value);
    Stream.Position := 0;
    {$IF RTLVersion >= 35.0}
    TNetEncoding.Base64String.Encode(Stream, StreamOutput);
    {$ELSE}
    TNetEncoding.Base64.Encode(Stream, StreamOutput);
    {$ENDIF}
    Result := StreamOutput.DataString;
  finally
    Stream.Free;
    StreamOutput.Free;
  end;
end;

procedure DecodeBase64ToFile(const Base64Str: string; const FileName: string);
begin
  {--- Convert Base64 string to byte array for input stream }
  var Bytes := TEncoding.UTF8.GetBytes(Base64Str);

  {--- Create the flows }
  var InputStream := TBytesStream.Create(Bytes);
  var OutputStream := TFileStream.Create(FileName, fmCreate);
  try
    {--- Decode using TNetEncoding.Base64.Decode (stream) }
    TNetEncoding.Base64.Decode(InputStream, OutputStream);
  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;

procedure DecodeBase64ToStream(const Base64Str: string; const Stream: TStream);
begin
  {--- Converts the base64 string directly into the memory stream }
  var InputStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(Base64Str));
    try
      TNetEncoding.Base64.Decode(InputStream, Stream);
      Stream.Position := 0;
    finally
      InputStream.Free;
    end;
end;

end.
