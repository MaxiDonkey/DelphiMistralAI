# Libraries API - Main

- [Introduction](#introduction)
- [List all libraries you have access to](#list-all-libraries-you-have-access-to)
- [Create a new Library](#create-a-new-library)
- [Detailed information about a specific Library](#detailed-information-about-a-specific-library)
- [Delete a library and all of it's document](#delete-a-library-and-all-of-its-document)
- [Update a library](#update-a-library)

<br>

___

## Introduction

A ***Library*** is a logical container and access interface for curated knowledge used by agents to augment their reasoning. It encapsulates collections of content, supports listing, sorting, and search semantics, and serves as the primary entry point for pulling in relevant context. Libraries enable dynamic, contextual enrichment without prescribing the underlying content format, making them a flexible backbone for RAG-enabled systems.

<br>

___

## List all libraries you have access to

ist all libraries that you have created or have been shared with you.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.LibrariesMain.AsyncAwaitList;

  promise
    .&Then<TLibrariesMainList>(
      function (Value: TLibrariesMainList): TLibrariesMainList
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
//  Client.LibrariesMain.AsyncList(
//    function : TAsyncLibrariesMainList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesMain.List;
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Create a new Library

Create a new Library, you will be marked as the owner and only you will have the possibility to share it with others. When first created this will only be accessible by you.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;
  
  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.LibrariesMain.AsyncAwaitCreate(
    procedure (Params: TLibrariesMainParams)
    begin
      Params.Name('first lib');
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TLibrariesMain>(
      function (Value: TLibrariesMain): TLibrariesMain
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
//  Client.LibrariesMain.AsyncCreate(
//    procedure (Params: TLibrariesMainParams)
//    begin
//      Params.Name('first lib');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncLibrariesMain
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesMain.Create(
//    procedure (Params: TLibrariesMainParams)
//    begin
//      Params.Name('first lib');
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

## Detailed information about a specific Library

Given a library id, details information about that Library.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28

  //Asynchronous promise example
  var Promise := Client.LibrariesMain.AsyncAwaitRetrieve(lib_id);

  promise
    .&Then<TLibrariesMain>(
      function (Value: TLibrariesMain): TLibrariesMain
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
//  Client.LibrariesMain.AsyncRetrieve(lib_id,
//    function : TAsyncLibrariesMain
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesMain.Retrieve(lib_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Delete a library and all of it's document

Given a library id, deletes it together with all documents that have been uploaded to that library.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28

  //Asynchronous promise example
  var Promise := Client.LibrariesMain.AsyncAwaitDelete(lib_id);

  promise
    .&Then<TLibrariesMain>(
      function (Value: TLibrariesMain): TLibrariesMain
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
//  Client.LibrariesMain.AsyncDelete(lib_id,
//    function : TAsyncLibrariesMain
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesMain.Delete(lib_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Update a library

Given a library id, you can update the name and description.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var lib_id := 'lib_id_to_retrieve'; //e.g. 0198502a-6f55-778e-6978-470167da0f28

  //Asynchronous promise example
  var Promise := Client.LibrariesMain.AsyncAwaitUpdate(lib_id,
    procedure (Params: TUpdateLibrariesMainParams)
    begin
      Params.Name('vector modified');
      Params.Description('librarie modified');
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TLibrariesMain>(
      function (Value: TLibrariesMain): TLibrariesMain
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
//  Client.LibrariesMain.AsyncUpdate(lib_id,
//    procedure (Params: TUpdateLibrariesMainParams)
//    begin
//      Params.Name('fisrt vector modified');
//      Params.Description('librarie modified');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncLibrariesMain
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.LibrariesMain.Update(lib_id,
//    procedure (Params: TUpdateLibrariesMainParams)
//    begin
//      Params.Name('fisrt vector modified');
//      Params.Description('librarie modified');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```



