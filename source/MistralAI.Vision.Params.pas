unit MistralAI.Vision.Params;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils;

type
  /// <summary>
  /// The <c>TVisionSource</c> record represents an image source used in the vision system.
  /// The source can either be a URL linking to an image or the image itself encoded in Base64 format.
  /// </summary>
  /// <remarks>
  /// The <c>TVisionSource</c> record is responsible for holding the image data and validating its format.
  /// It supports two types of image sources: a secure HTTPS URL or a Base64-encoded image.
  /// The record includes methods to create and validate these sources.
  /// Example usage:
  /// <code>
  /// var Source: TVisionSource;
  /// Source := TVisionSource.Create('https://example.com/image.jpg');
  /// or
  /// Source := TVisionSource.Create('c:\my_folder\my_file.webp');
  /// </code>
  /// </remarks>
  TVisionSource = record
  private
    FSource: string;
    FValue: WideString;
    procedure FileCheck;
  public
    /// <summary>
    /// The value field must be either a URL linking to the image or the image itself
    /// in Base64 format, included with the request.
    /// </summary>
    property Data: WideString read FValue write FValue;
    /// <summary>
    /// Creates an instance of <c>TVisionSource</c> based on the provided value.
    /// This method determines whether the provided value is a secure HTTPS URL or a Base64-encoded image.
    /// </summary>
    /// <param name="Value">
    /// The value representing the image source, which must either be a URL (starting with "https") or a Base64-encoded string.
    /// If the value is a URL, it must use HTTPS; otherwise, an exception will be raised.
    /// </param>
    /// <returns>
    /// A <c>TVisionSource</c> object that contains the image source and its type (either a URL or a Base64-encoded image).
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the provided URL is not secure (i.e., does not use HTTPS).
    /// </exception>
    /// <remarks>
    /// This function verifies if the provided value starts with "https". If it does, it treats the value as a URL.
    /// If the value does not begin with "https" but starts with "http", an exception is raised to enforce a secure connection.
    /// If neither condition is met, the method attempts to handle the value as a Base64-encoded image or a file.
    /// </remarks>
    class function Create(const Value: string): TVisionSource; overload; static;
  end;

implementation

uses
  MistralAI.NetEncoding.Base64;

{ TVisionSource }

class function TVisionSource.Create(const Value: string): TVisionSource;
begin
  {--- Set value into the source }
  Result.FSource := Value;

  {--- Extract the first five chars into the signal variable }
  var Signal := EmptyStr;
  if Value.Length >= 5 then
    Signal := AnsiLowerCase(Copy(Value, 1, 5));

  {--- Check URL secure }
  if Signal = 'https' then
    begin
      Result.Data := Value;
    end
  else
    begin
      {--- Raise an exception on URL not secure }
      if (Value.Length >= 4) and (AnsiLowerCase(Copy(Value, 1, 4))  = 'http') then
        raise Exception.Create('Invalid URL: Secure HTTPS connection required');

      {--- handle the value as a Base64-encoded image or a file}
      Result.FileCheck;
    end;
end;

procedure TVisionSource.FileCheck;
begin
  var MimeType := ResolveMimeType(FSource);

  if IndexStr(MimeType, ['image/png', 'image/jpeg', 'image/gif', 'image/webp']) = -1 then
    raise Exception.Create('Unsupported image format');

  Data :=  Format('data:%s;base64,%s', [MimeType, EncodeBase64(FSource)]);
end;

end.
