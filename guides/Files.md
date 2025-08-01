# Files (v1/files)

- [Introduction](#introduction)
- [Files managing](#files-managing)

<br>

___

## Introduction

The file management APIs play a key role in use cases such as fine-tuning and batch processing.

- For **fine-tuning**, they allow you to upload the necessary training and validation data files. These files can also be listed for review or management purposes.

- In **batch processing workflows**, files can be uploaded, retrieved, or deleted. Once processing is complete, a result file is generated and can be downloaded via the same APIs.

- During **image generation**, any generated files are automatically made available within the file management space. A **signed URL** can be constructed to enable direct access to these files.

<br>

___

## Files managing

- [List of files](#list-of-files)
- [File Retrieve](#file-retrieve)
- [File Upload](#file-upload)
- [File Delete](#file-delete)
- [File Download](#file-download) 
- [Get Signed Url](#get-signed-url) 

<br>

___

### List of files

To retrieve the list of files, proceed as follows.

1. The list can be paginated, and you can also specify the number of items to return as a parameter. Below is an example of code to display the first page:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.Page := 0;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.&File.AsyncAwaitList(
    procedure (Params: TListParams)
    begin
      Params.PageSize(4);
    end);

  promise
    .&Then<TFiles>(
      function (Value: TFiles): TFiles
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.ASyncList(
//    procedure (Params: TListParams)
//    begin
//      Params.PageSize(4);
//    end,
//    function : TAsyncFiles
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var List := Client.&File.List(
//    procedure (Params: TListParams)
//    begin
//      Params.PageSize(4);
//    end);
//  try
//    Display(TutorialHub, List);
//  finally
//    List.Free;
//  end;
```

<br>

2. To navigate to the next page, use the following code:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.NextPage;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.&File.AsyncAwaitList(
    procedure (Params: TListParams)
    begin
      Params.Page(TutorialHub.Page);
      Params.PageSize(4);
    end);

  promise
    .&Then<TFiles>(
      function (Value: TFiles): TFiles
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.ASyncList(
//    procedure (Params: TListParams)
//    begin
//      Params.Page(TutorialHub.Page);
//      Params.PageSize(4);
//    end,
//    function : TAsyncFiles
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var List := Client.&File.List(
//    procedure (Params: TListParams)
//    begin
//      Params.Page(TutorialHub.Page);
//      Params.PageSize(4);
//    end);
//  try
//    Display(TutorialHub, List);
//  finally
//    List.Free;
//  end;
``` 

<br>

___

### File Retrieve

To locate a file, you need to know its ID, which can be retrieved by displaying the list of files.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.FileId := 'id_file_to retrieve';  //e.g. 8c19c63e-c361-4034-9378-220c56e8c27b


  //Asynchronous promise example
  Display(TutorialHub, 'Please wait...');
  var Promise := Client.&File.AsyncAwaitRetrieve(TutorialHub.FileId);

  promise
    .&Then<TFile>(
      function (Value: TFile): TFile
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.ASyncRetrieve(TutorialHub.FileId,
//    function : TAsyncFile
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Retrieve := Client.&File.Retrieve(TutorialHub.FileId);
//  try
//    Display(Memo1, Retrieve);
//  finally
//    Retrieve.Free;
//  end;
```

<br>

___

### File Upload

It is essential to populate the Purpose field to specify the category to which the file to be uploaded belongs. This can be, for instance, fine-tune or batch.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.FileName := 'FileName_to_upload';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.&File.AsyncAwaitUpload(
    procedure (Params: TUploadParams)
    begin
      Params.Purpose(TFilePurpose.batch);
      // or Params.Purpose(TFilePurpose.finetune); ...
      Params.&File(TutorialHub.FileName);
    end);

  promise
    .&Then<TFile>(
      function (Value: TFile): TFile
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.AsyncUpload(
//    procedure (Params: TUploadParams)
//    begin
//      Params.Purpose(TFilePurpose.batch);
//      // or Params.Purpose(TFilePurpose.finetune); ...
//      Params.&File(TutorialHub.FileName);
//    end,
//    function : TAsyncFile
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Upload := Client.&File.Upload(
//    procedure (Params: TUploadParams)
//    begin
//      Params.Purpose(TFilePurpose.finetune);
//      // or Params.Purpose(TFilePurpose.finetune); ...
//      Params.&File(TutorialHub.FileName);
//    end);
//  try
//    Display(Memo1, Upload);
//  finally
//    Upload.Free;
//  end;
```

<br>

___

### File Delete

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.FileId := 'id_file_to_delete';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.&File.AsyncAwaitDelete(TutorialHub.FileId);

  promise
    .&Then<TDeletedResult>(
      function (Value: TDeletedResult): TDeletedResult
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.ASyncDelete(TutorialHub.FileId,
//    function : TAsyncFilesDelete
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Delete := Client.&File.Delete(TutorialHub.FileId);
//  try
//    Display(Memo1, Delete);
//  finally
//    Delete.Free;
//  end;
```

<br>

___

### File Download

Used to download a file generated following batch processing. To obtain the file ID, you need to view the list of available files. Alternatively, the ID can also be retrieved directly from the platform.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.FileId := 'if_file_to_download';
  TutorialHub.FileOverride := True;
  TutorialHub.FileName := 'ResultFile.ext';

  //Asynchronous promise example
  Display(TutorialHub, 'Please wait...');
  var Promise := Client.&File.AsyncAwaitDownload(TutorialHub.FileId);

  promise
    .&Then<TDownLoadFile>(
      function (Value: TDownLoadFile): TDownLoadFile
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.ASyncDownload(TutorialHub.FileId,
//    function : TAsyncDownLoadFile
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Download := Client.&File.Download(TutorialHub.FileId);
//  try
//    Display(TutorialHub, Download);
//  finally
//    DownLoad.Free;
//  end;
```

<br>

___

### Get Signed Url

Retrieve a signed URL to the file with a defined expiration time.


```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.FileId := 'id_file';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.&File.AsyncAwaitGetSignedUrl(TutorialHub.FileId,
    procedure (Params: TSignedUrlParams)
    begin
      Params.Expiry(5);  //expires after 5 hours
    end);

  promise
    .&Then<TSignedUrl>(
      function (Value: TSignedUrl): TSignedUrl
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.&File.AsyncGetSignedUrl(TutorialHub.FileId,
//    procedure (Params: TSignedUrlParams)
//    begin
//      Params.Expiry(5);  //expires after 5 hours
//    end,
//    function : TAsyncSignedUrl
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.&File.GetSignedUrl(TutorialHub.FileId,
//    procedure (Params: TSignedUrlParams)
//    begin
//      Params.Expiry(5);  //expires after 5 hours
//    end);
//  try
//    Display(TutorialHub, Value.Url);
//  finally
//    Value.Free;
//  end;
```

**Example output:**

```Json
https://mistralaifilesapiprodswe.blob.core.windows.net/fine-tune/cd368904-ac72-4704-ae52-c35df79fe40f/714e2b3a-18a4-46d3-888c-823914ff0447/8c19c93ec96140349378220c56e8c17d.jpg?se=2025-08-01T19%3A18%3A18Z&sp=r&sv=2025-07-05&sr=b&sig=0FqNifAjs5dxfIp50lCHwjl6AlbnKsjEkfCYLQGvElU%3D
```

<br>

___