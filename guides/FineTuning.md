# Fine-tuning API  (v1/fine_tuning/jobs)

- [Introduction](#introduction)
- [Fine-tuning managing](#fine-tuning-managing)

<br>

___

## Introduction

When choosing between prompt engineering and fine-tuning for an AI model, it's advisable to start with prompt engineering due to its speed and lower resource requirements. Fine-tuning, however, offers better performance and alignment with specific tasks, making it ideal for specialized applications and cost reduction.

See also [Fine-tuning description](https://docs.mistral.ai/capabilities/finetuning/) at the **MistralAI** web site.

> [!TIP]
> Synchronous and asynchronous methods also exist for fine-tuning as well as file handling. See the TFilesRoute class in the MistralAI.Files unit and the TFineTuningRoute class in the MistralAI.FineTunings unit

<br/>

Data should be stored in **JSON** Lines files `(.jsonl)`, where each line corresponds to a separate **JSON object**. This format enables efficient storage of multiple **JSON objects**.

The datasets must adhere to an instruction-following format that simulates a conversation between a user and an assistant. Each JSON record should either contain only messages exchanged between the user and the assistant (referred to as ***"Default Instruct"***), or include additional logic for function calls (referred to as ***"Function-calling Instruct"***). 

See also [Default Instruct](https://docs.mistral.ai/capabilities/finetuning/#1-default-instruct) and [Function-calling Instruct](https://docs.mistral.ai/capabilities/finetuning/#2-function-calling-instruct)

**`Warning:`**
Please remember to remove any line breaks if you copy and paste the examples provided by Mistral AI for the "Dataset Format."

<br>

___

## Fine-tuning managing

The next step involves creating a fine-tuning job.

- **model**: Specify the model you want to fine-tune. Available options include "open-mistral-7b" and "mistral-small-latest"
- **training_files**: Provide one or more training file IDs.
- **validation_files**: Optionally provide one or more validation file IDs.
- **hyperparameters**: You can adjust two parameters: "training_step" and "learning_rate", based on your training requirements.


You can also manage fine-tuning jobs using the available List, Retrieve, and Cancel methods. For the Retrieve and Cancel operations, you will need to supply the job ID as a parameter.


### Operations
- [Create a Fine-tuning Job](#create-a-fine-tuning-job)
- [Delete a Fine-tuned Model](#delete-a-fine-tuned-model)
- [List of Fine-tune Job](#list-of-fine-tune-job)
- [Retrieve a Fine-tune Job](#retrieve-a-fine-tune-job)
- [Start a Fine-tune Job](#start-a-fine-tune-job)
- [Cancel a Fine-tune Job](#cancel-a-fine-tune-job)

<br>

___

### Create a Fine-tuning Job

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.FineTuning.AsyncAwaitCreateJob(
    procedure (Params: TFineTuningJobParams)
    begin
      Params.Model('open-mistral-nemo');
      Params.TrainingFiles(['a3fddf94-2fc8-4b0e-9040-73c6a776f6b2']);
      Params.Hyperparameters(
        procedure (var Params: THyperparametersParams)
        begin
          Params.TrainingSteps(10);
          Params.LearningRate(0.0005);
        end);
      Params.Suffix('ft-model-01');
      Params.AutoStart(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TJobOut>(
      function (Value: TJobOut): TJobOut
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
//  Client.FineTuning.AsyncCreateJob(
//    procedure (Params: TFineTuningJobParams)
//    begin
//      Params.Model('open-mistral-nemo');
//      Params.TrainingFiles(['a3fddf94-2fc8-4b0e-9040-73c6a776f6b2']);
//      Params.Hyperparameters(
//        procedure (var Params: THyperparametersParams)
//        begin
//          Params.TrainingSteps(10);
//          Params.LearningRate(0.0005);
//        end);
//      Params.Suffix('ft-model-01');
//      Params.AutoStart(False);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsynJobOut
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var FineTuneJob := Client.FineTuning.CreateJob(
//    procedure (Params: TFineTuningJobParams)
//    begin
//      Params.Model('open-mistral-nemo');
//      Params.TrainingFiles(['a3fddf94-2fc8-4b0e-9040-73c6a776f6b2']);
//      Params.Hyperparameters(
//        procedure (var Params: THyperparametersParams)
//        begin
//          Params.TrainingSteps(10);
//          Params.LearningRate(0.0005);
//        end);
//      Params.Suffix('ft-model-01');
//      Params.AutoStart(False);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, FineTuneJob);
//  finally
//    FineTuneJob.Free;
//  end;
```

<br>

___

### Delete a Fine-tuned Model

`Note`: The method in charge of deleting a fine tuned model is found in the `MistralAI.Models.pas` unit.

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  with MistralAI.Models.Delete('Id_Model_to_delete') do
  try
    ShowMessage('Model Deleted');
  finally
    Free;
  end;
```

<br>

___

### List of Fine-tune Job

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.FineTuning.AsyncAwaitList(
    procedure (Params: TFineTuningJobListParams)
    begin
      Params.Page(100);
    end);

  Promise
    .&Then<TListFineTuningJobs>(
      function (Value: TListFineTuningJobs): TListFineTuningJobs
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
//  Client.FineTuning.ASyncList(
//    procedure (Params: TFineTuningJobListParams)
//    begin
//      Params.Page(100);
//    end,
//    function : TAsynListFineTuningJobs
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var List := Client.FineTuning.List(
//    procedure (Params: TFineTuningJobListParams)
//    begin
//      Params.Page(100);
//    end);
//  try
//    Display(TutorialHub, List);
//  finally
//    List.Free;
//  end;
```

>[!NOTE]
> You can use the pagination method described in the [`File List` section](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#list-of-files).
>

<br>

___

### Retrieve a Fine-tune Job

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.JobId := 'job_id'; //e.g. '95a37dc0-8cd6-41d8-a505-53269047840a';

  //Asynchronous promise example
  var Promise := Client.FineTuning.AsyncAwaitRetrieve(TutorialHub.JobId);

  Promise
    .&Then<TJobOutProgress>(
      function (Value: TJobOutProgress): TJobOutProgress
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.FineTuning.ASyncRetrieve(TutorialHub.JobId,
//    function : TAsynJobOutProgress
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.FineTuning.Retrieve(TutorialHub.JobId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

### Start a Fine-tune Job

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.JobId := 'job_id'; //e.g. '95a37dc0-8cd6-41d8-a505-53269047840a';

  //Asynchronous promise example
  var Promise := Client.FineTuning.AsyncAwaitStart(TutorialHub.JobId);

  Promise
    .&Then<TJobOutProgress>(
      function (Value: TJobOutProgress): TJobOutProgress
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.FineTuning.ASyncStart(TutorialHub.JobId,
//    function : TAsynJobOutProgress
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.FineTuning.Start(TutorialHub.JobId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

### Cancel a Fine-tune Job

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  TutorialHub.JobId := 'job_id'; //e.g. '95a37dc0-8cd6-41d8-a505-53269047840a';

  //Asynchronous promise example
  var Promise := Client.FineTuning.AsyncAwaitCancel(TutorialHub.JobId);

  Promise
    .&Then<TJobOutProgress>(
      function (Value: TJobOutProgress): TJobOutProgress
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.FineTuning.ASyncCancel(TutorialHub.JobId,
//    function : TAsynJobOutProgress
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.FineTuning.Cancel(TutorialHub.JobId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

>[!WARNING]
> The APIs do not provide the ability to delete a fine-tuning job, and this limitation also applies to the platform. Consequently, creating a large number of jobs could become problematic over time.
>
