﻿unit MistralAI.Httpx;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.Classes, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.NetEncoding;

type
  /// <summary>
  /// THttpx provides utility methods for handling HTTP-related tasks such as
  /// downloading data, encoding it in Base64, and retrieving MIME types.
  /// </summary>
  THttpx = class
    /// <summary>
    /// Extracts the file name from the specified URI, omitting any query string.
    /// </summary>
    /// <param name="URI">
    /// The full URI (optionally containing a '?' query portion) from which to extract the file name.
    /// </param>
    /// <returns>
    /// The substring after the last '/' in the URI, without any query parameters.
    /// </returns>
    class function GetFileNameFromURI(const URI: string): string;

    /// <summary>
    /// Launches the specified file or application using the default system handler.
    /// </summary>
    /// <param name="FilePath">
    /// The full path of the file or executable to open.
    /// </param>
    /// <exception cref="EOSError">
    /// Raised if the ShellExecute call fails (i.e. returns a value ≤ 32).
    /// </exception>
    class procedure Open(const FilePath: string);

    /// <summary>
    /// Downloads the file at the specified signed URL into the current directory,
    /// using the file name derived from the URI, and optionally opens it.
    /// </summary>
    /// <param name="Uri">
    /// The signed URL pointing to the file to download.
    /// </param>
    /// <param name="Open">
    /// If <c>True</c>, automatically open the downloaded file after the download completes;
    /// otherwise leave it saved only to disk. Defaults to <c>False</c>.
    /// </param>
    /// <exception cref="ENetHTTPClientException">
    /// Raised if the HTTP download fails.
    /// </exception>
    /// <exception cref="EOSError">
    /// Raised if opening the file fails (ShellExecute returns ≤ 32).
    /// </exception>
    class procedure DownloadFromSignedUrl(const Uri: string; const Open: Boolean = False); overload;

    /// <summary>
    /// Downloads the file from the specified signed URL into the given directory,
    /// using the file name derived from the URI, and optionally opens it.
    /// </summary>
    /// <param name="Uri">
    /// The signed URL pointing to the file to download.
    /// </param>
    /// <param name="Path">
    /// The target directory in which to save the file. If empty, the current directory is used.
    /// </param>
    /// <param name="Open">
    /// If <c>True</c>, automatically open the downloaded file after saving;
    /// otherwise leave it saved only to disk. Defaults to <c>False</c>.
    /// </param>
    /// <exception cref="ENetHTTPClientException">
    /// Raised if the HTTP download fails.
    /// </exception>
    /// <exception cref="EOSError">
    /// Raised if opening the file fails (ShellExecute returns ≤ 32).
    /// </exception>
    class procedure DownloadFromSignedUrl(const Uri: string; const Path: string; const Open: Boolean = False); overload;
  end;

implementation

{ THttpx }

class procedure THttpx.DownloadFromSignedUrl(const Uri: string;
  const Path: string;
  const Open: Boolean);
var
  HttpClient: THTTPClient;
  FileStream: TFileStream;
  FilePath: string;
begin
  FilePath := GetFileNameFromURI(Uri);
  if not Path.IsEmpty then
    FilePath := Format('%s\%s', [Path, FilePath]);

  HttpClient := THTTPClient.Create;
  try
    FileStream := TFileStream.Create(FilePath, fmCreate);
    try
      HttpClient.Get(URI, FileStream);
      if Open then
        THttpx.Open(FilePath);
    finally
      FileStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class procedure THttpx.DownloadFromSignedUrl(const Uri: string;
  const Open: Boolean);
begin
  DownloadFromSignedUrl(Uri, '', Open);
end;

class function THttpx.GetFileNameFromURI(const URI: string): string;
begin
  var CleanURI := URI.Split(['?'])[0];
  Result := Copy(CleanURI, LastDelimiter('/', CleanURI) + 1, MaxInt);
end;

class procedure THttpx.Open(const FilePath: string);
var
  Ret: HINST;
begin
  Ret := ShellExecuteW(0, nil, PWideChar(FilePath), nil, nil, SW_SHOWNORMAL);
  if Ret <= 32 then
    RaiseLastOSError;
end;

end.
