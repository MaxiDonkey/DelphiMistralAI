# Batch Inference (v1/batch/jobs)

- [Introduction](#introduction)
- [List of batch](#list-of-batch)
- [Batch job creation](#batch-job-creation)
- [Batch job cancellation](#batch-job-cancellation)
- [Batch job retrieve](#batch-job-retrieve)
- [Batch job result file](#batchjob-result-file)

<br>

___

## Introduction

The batch processing feature allows sending a large number of requests simultaneously to various endpoints (chat, embeddings, moderations, etc.). Each batch contains uniquely identified requests and is processed asynchronously. This enables tracking the progress of the processing, accessing results once completed, and handling potential timeouts.

Requests can be customized using metadata, and it is possible to view or cancel ongoing processes. Final results are available as downloadable files and remain accessible afterward.

This approach provides more efficient management of high volumes of requests without imposing a strict limit on the number of batches, while adhering to an overall limit on pending requests. It is particularly useful for automating and consolidating processing, especially in cases of large-scale or repetitive requests.

Refer to the [official documentation](https://docs.mistral.ai/capabilities/batch/).

<br>

___

## List of batch

The list of batch processes can be retrieved using the following API. The pagination mechanism described for the [file list](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#list-of-files) can also be used.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Batch.AsyncAwaitList(
    procedure (Params: TBatchJobListParams)
    begin
      Params.PageSize(100);
    end);

  promise
    .&Then<TBatchJobList>(
      function (Value: TBatchJobList): TBatchJobList
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
//  Client.Batch.ASyncList(
//    procedure (Params: TBatchJobListParams)
//    begin
//      Params.PageSize(100);
//    end,
//    function : TAsynBatchJobList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var List := Client.Batch.List(
//    procedure (Params: TBatchJobListParams)
//    begin
//      Params.PageSize(100);
//    end);
//  try
//    Display(TutorialHub, List);
//  finally
//    List.Free;
//  end;
```

<br>

___

## Batch job creation

We take the batch given in the example on the official site

Here's an example of how to structure a batch request:

```Json
{"custom_id": "0", "body": {"max_tokens": 100, "messages": [{"role": "user", "content": "What is the best French cheese?"}]}}
{"custom_id": "1", "body": {"max_tokens": 100, "messages": [{"role": "user", "content": "What is the best French wine?"}]}}
```

<br/>

1. Save this text in a file and name it as you prefer.
2. Next, upload the file using the code provided in the [File Upload section](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#file-upload).
3. Be sure to set the value of Purpose to batch.
4. Note the file ID after it has been uploaded, as this `ID` will be required when creating the batch processing job.

<br/>

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Display(TutorialHub, 'This may take a few seconds.');
  var Promise := Client.Batch.AsyncAwaitCreateJob(
    procedure (Params: TBatchJobParams)
    begin
      Params.InputFiles(['229744f2-67fd-422a-bd32-4c6ab5ea3c44']);
      Params.Model('mistral-large-latest');
      Params.Endpoint(TEndPointType.ChatCompletion);
      Params.Metadata(TJSONObject.Create.AddPair('job_type', 'texting'));
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<TBatchJob>(
      function (Value: TBatchJob): TBatchJob
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
//  Client.Batch.ASyncCreateJob(
//    procedure (Params: TBatchJobParams)
//    begin
//      Params.InputFiles(['229744f2-67fd-422a-bd32-4c6ab5ea3c44']);
//      Params.Model('mistral-large-latest');
//      Params.Endpoint(TEndPointType.ChatCompletion);
//      Params.Metadata(TJSONObject.Create.AddPair('job_type', 'texting'));
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynBatchJob
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Batch.CreateJob(
//    procedure (Params: TBatchJobParams)
//    begin
//      Params.InputFiles(['229744f2-67fd-422a-bd32-4c6ab5ea3c44']);
//      Params.Model('mistral-large-latest');
//      Params.Endpoint(TEndPointType.epChatCompletion);
//      Params.Metadata(TJSONObject.Create.AddPair('job_type', 'texting'));
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

>[!NOTE]
> Note: The following metadata was specified during creation as a label: 
>```Json
> {"job_type": "texting"}
>```


<br>

___

## Batch job cancellation

A job can be interrupted as long as it is still being processed. To perform this action, ensure that you have obtained the job ID beforehand, then use the following code:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.BatchId := 'batch_id'; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Batch.AsyncAwaitCancel(TutorialHub.BatchId);

  promise
    .&Then<TBatchJob>(
      function (Value: TBatchJob): TBatchJob
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
//  Client.Batch.ASyncCancel(TutorialHub.BatchId,
//    function : TAsynBatchJob
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Batch.Cancel(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Batch job retrieve

A batch can be retrieved using its ID, allowing you to check its status to track progress or obtain the ID of the file generated at the end of the process containing the expected results. To do so, use the following code:

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.BatchId := 'batch_id'; //e.g. 982d2bbc-adbb-4c6e-b072-5aec973b4c86

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Batch.AsyncAwaitRetrieve(TutorialHub.BatchId);

  promise
    .&Then<TBatchJob>(
      function (Value: TBatchJob): TBatchJob
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
//  Client.Batch.ASyncRetrieve(TutorialHub.BatchId,
//    function : TAsynBatchJob
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Batch.Retrieve(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Batch job result file

If the batch processing completes successfully, an ID pointing to a file containing the results is provided. This ID can be accessed through the OutputFile field of the TBatchJob class.
In this case, you can use the [File Download API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#file-download), demonstrated in this code example, to retrieve the file.
Alternatively, you can download the file directly from the platform.