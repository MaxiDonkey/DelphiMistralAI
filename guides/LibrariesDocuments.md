# Libraries API - Documents

- [Intoduction](#intoduction)
- [List document in a given library](#list-document-in-a-given-library)
- [Upload a new document](#upload-a-new-document)
- [Retrieve the metadata of a specific document](#retrieve-the-metadata-of-a-specific-document)
- [Update the metadata of a specific document](#update-the-metadata-of-a-specific-document)
- [Delete a document](#delete-a-document)
- [Retrieve the text content of a specific document](#retrieve-the-text-content-of-a-specific-document)
- [Retrieve the processing status of a specific document](#retrieve-the-processing-status-of-a-specific-document)
- [Retrieve the signed URL of a specific document](#retrieve-the-signed-url-of-a-specific-document)
- [Retrieve the signed URL of text extracted from a given document](#retrieve-the-signed-url-of-text-extracted-from-a-given-document)
- [Reprocess a document](#reprocess-a-document)

<br>

___

## Intoduction

***Document Library*** is a native connector that gives agents direct access to your documents hosted in **Mistral Cloud**. It serves as a dynamic knowledge base: you can list, sort, search, and leverage the documents to strengthen agent capabilities via a RAG integration. The library supports collection management, browsing recent documents, and easily fits into your existing workflows while respecting access controls and security best practices.

For detailed information on creating, modifying, deleting, and listing documents, please refer to the [Main API Libraries](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesMain.md#libraries-api---main) section.

<br>

___

## List document in a given library

Given a library, lists the document that have been uploaded to that library.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_list_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28


  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitList(lib_id,
    procedure (Params: TLibrariesDocumentsUrlParams)
    begin
      Params.PageSize(100);
      Params.Page(0);
      TutorialHub.JSONRequest := Params.Value;
    end);

  Promise
    .&Then<TLibrariesDocumentsList>(
      function (Value: TLibrariesDocumentsList): TLibrariesDocumentsList
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
//  Client.LibrariesDocuments.AsyncList(lib_id,
//    procedure (Params: TLibrariesDocumentsUrlParams)
//    begin
//      Params.PageSize(100);
//      Params.Page(0);
//      TutorialHub.JSONRequest := Params.Value;
//    end,
//    function : TAsyncLibrariesDocumentsList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.List(lib_id,
//    procedure (Params: TLibrariesDocumentsUrlParams)
//    begin
//      Params.PageSize(100);
//      Params.Page(0);
//      TutorialHub.JSONRequest := Params.Value;
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Upload a new document

Given a library, upload a new document to that library. It is queued for processing, it status will change it has been processed. The processing has to be completed in order be discoverable for the library search

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_upload_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitUpload(lib_id,
    procedure (Params: TLibrariesDocumentsUploadParams)
    begin
      Params.&File('..\..\sample\File_Search_file.pdf');
      TutorialHub.JSONRequest := 'multipart/form-data';
    end);

  Promise
    .&Then<TLibrariesDocuments>(
      function (Value: TLibrariesDocuments): TLibrariesDocuments
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
//  Client.LibrariesDocuments.AsyncUpload(lib_id,
//    procedure (Params: TLibrariesDocumentsUploadParams)
//    begin
//      Params.&File('..\..\sample\File_Search_file.pdf');
//      TutorialHub.JSONRequest := 'multipart/form-data';
//    end,
//    function : TAsyncLibrariesDocuments
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);


  //Synchronous example
//  var Value := Client.LibrariesDocuments.Upload(lib_id,
//    procedure (Params: TLibrariesDocumentsUploadParams)
//    begin
//      Params.&File('..\..\sample\File_Search_file.pdf');
//      TutorialHub.JSONRequest := 'multipart/form-data';
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Retrieve the metadata of a specific document

Given a library and a document in this library, you can retrieve the metadata of that document.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_get_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitRetrieve(lib_id, upload_id);

  Promise
    .&Then<TLibrariesDocuments>(
      function (Value: TLibrariesDocuments): TLibrariesDocuments
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
//  Client.LibrariesDocuments.AsyncRetrieve(lib_id, upload_id,
//    function : TAsyncLibrariesDocuments
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.Retrieve(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Update the metadata of a specific document

Given a library and a document in that library, update the name of that document.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_update_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitUpdate(lib_id, upload_id,
    procedure (Params: TLibrariesDocumentsUpdateParams)
    begin
      Params.Name('Filename update');
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TLibrariesDocuments>(
      function (Value: TLibrariesDocuments): TLibrariesDocuments
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
//  Client.LibrariesDocuments.AsyncUpdate(lib_id, upload_id,
//    procedure (Params: TLibrariesDocumentsUpdateParams)
//    begin
//      Params.Name('Filename update');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncLibrariesDocuments
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.Update(lib_id, upload_id,
//    procedure (Params: TLibrariesDocumentsUpdateParams)
//    begin
//      Params.Name('Filename update');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Delete a document

Given a library and a document in that library, delete that document. The document will be deleted from the library and the search index.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_delete_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitDelete(lib_id, upload_id);

  Promise
    .&Then<TLibraryDocumentsProcessed>(
      function (Value: TLibraryDocumentsProcessed): TLibraryDocumentsProcessed
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
//  Client.LibrariesDocuments.AsyncDelete(lib_id, upload_id,
//    function : TAsyncLibraryDocumentsProcessed
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.Delete(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Retrieve the text content of a specific document

Given a library and a document in that library, you can retrieve the text content of that document if it exists. For documents like pdf, docx and pptx the text content results from our processing using Mistral OCR.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_get_text_content_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitRetrieveText(lib_id, upload_id);

  Promise
    .&Then<TLibraryDocumentsText>(
      function (Value: TLibraryDocumentsText): TLibraryDocumentsText
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
//  Client.LibrariesDocuments.AsyncRetrieveText(lib_id, upload_id,
//    function : TAsyncLibraryDocumentsText
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.RetrieveText(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Retrieve the processing status of a specific document

Given a library and a document in that library, retrieve the processing status of that document.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_get_status_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitRetrieveStatus(lib_id, upload_id);

  Promise
    .&Then<TLibraryDocumentsStatus>(
      function (Value: TLibraryDocumentsStatus): TLibraryDocumentsStatus
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
//  Client.LibrariesDocuments.AsyncRetrieveStatus(lib_id, upload_id,
//    function : TAsyncLibraryDocumentsStatus
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.RetrieveStatus(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Retrieve the signed URL of a specific document

Given a library and a document in that library, retrieve the signed URL of a specific document.The url will expire after 30 minutes and can be accessed by anyone with the link.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_get_signed_url_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;
  
  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitGetDocumentSignedUrl(lib_id, upload_id);

  Promise
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
//  Client.LibrariesDocuments.AsyncGetDocumentSignedUrl(lib_id, upload_id,
//    function : TAsyncSignedUrl
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.GetDocumentSignedUrl(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Retrieve the signed URL of text extracted from a given document

Given a library and a document in that library, retrieve the signed URL of text extracted. For documents that are sent to the OCR this returns the result of the OCR queries.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_get_extracted_text_signed_url_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitGetTextSignedUrl(lib_id, upload_id);

  Promise
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
//  Client.LibrariesDocuments.AsyncGetTextSignedUrl(lib_id, upload_id,
//    function : TAsyncSignedUrl
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.GetTextSignedUrl(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Reprocess a document

Given a library and a document in that library, reprocess that document, it will be billed again.

Refer to [official documentation](https://docs.mistral.ai/api/#tag/beta.libraries.documents/operation/libraries_documents_reprocess_v1)

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'my_lib_id'; //e.g. 0198502a-6f55-778e-6978-470167da0f28
  var upload_id := 'my_upload_id'; //e.g. 3cece7413-3e66-42e4-9914-abdb42419dea

  //Asynchronous promise example
  var Promise := Client.LibrariesDocuments.AsyncAwaitReprocess(lib_id, upload_id);

  Promise
    .&Then<TLibraryDocumentsProcessed>(
      function (Value: TLibraryDocumentsProcessed): TLibraryDocumentsProcessed
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
//  Client.LibrariesDocuments.AsyncReprocess(lib_id, upload_id,
//    function : TAsyncLibraryDocumentsProcessed
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesDocuments.Reprocess(lib_id, upload_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___
