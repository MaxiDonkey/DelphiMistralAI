# **Delphi MistralAI API**

___
![Delphi async/await supported](https://img.shields.io/badge/Delphi%20async%2Fawait-supported-blue)
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.4/11/12-ffffba)
[![GetIt – Available](https://img.shields.io/badge/GetIt-Available-baffc9?logo=delphi&logoColor=white)](https://getitnow.embarcadero.com/mistralai-wrapper/)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-baffc9)
![GitHub](https://img.shields.io/badge/Updated%20on%20August%203,%202025-blue)

<br>

### NEW: 
- [Changelog](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Changelog.md) 
___

<br>

- [Introduction](#introduction)
- [Quickstart](#quickstart)
- [Documentation Overview](#documentation-overview)
- [TIPS for using the tutorial effectively](#tips-for-using-the-tutorial-effectively)
    - [Strategies for quickly using the code examples](#strategies-for-quickly-using-the-code-examples)
    - [Use file2knowledge](#use-file2knowledge)
- [DelphiMistralAI functional coverage](#delphimistralai-functional-coverage)
- [Conversations vs. Chat Completions](#conversations-vs-chat-completions)
- [Tips and tricks](#tips-and-tricks)
    - [How to prevent an error when closing an application while requests are still in progress?](#how-to-prevent-an-error-when-closing-an-application-while-requests-are-still-in-progress)
- [Contributing](#contributing)
- [License](#license)

___

<br>

# Introduction

> **Built with Delphi 12 Community Edition** (v12.1 Patch 1)  
>The wrapper itself is MIT-licensed.  
>You can compile and test it free of charge with Delphi CE; any recent commercial Delphi edition works as well.

<br>

**Core capabilities**  
- Unified access to text, vision and audio endpoints  
- Agentic workflows via the `v1/conversations` endpoint, with built-in tools `websearch`, `Code Interpreter`, `Image generation` and `Document library`.  
- Supports state-of-the-art models, including ***voxtral***, ***devstral***, ***mistral-ocr*** and the reasoning-centric ***magistral***

**Developer tooling**  
- Ready-made `Sync`, `Async`, and `Await` code snippets (TutorialHUB compatible)  
- Batch processing, function calling, file management, and content moderation out of the box  
- Built-in DUnit test helpers and a modular JSON configuration for streamlined setup  
- Mock-friendly design: the HTTP layer is injected via dependency injection, so you can swap in stubs or fakes for testing

<br>

> [!IMPORTANT]
>
> This is an unofficial library. **MistralAI** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [MistralAI](https://docs.mistral.ai/api/) public API.

<br>

___

# Quickstart

- [Account setup](#account-setup)
- [Obtain an api key](#obtain-an-api-key)
- [Getting started with Mistral AI API](#getting-started-with-mistral-ai-api)

<br>

## Account setup

To get started, head over to https://console.mistral.ai and either create a new Mistral account or sign in if you already have one. Once you’re in, open your Organization settings at https://admin.mistral.ai – this is where you’ll centralize billing and access control. In the Administration tab, locate the Billing section and enter your payment details to activate billing. From here you can manage all of your Workspaces as well as the broader Organization.

<br>

## Obtain an api key

To initialize the API instance, you need to obtain an [API key from MistralAI](https://admin.mistral.ai/organization/api-keys)

Once you have a token, you can initialize IMistralAI interface, which is an entry point to the API.

>[!NOTE]
>```Pascal
>//uses MistralAI, MistralAI.Types;
>
>//Declare 
>//  Client: IMistralAI
>//  ClientCoding: IMistralAI;
>
>  Client := TMistralAIFactory.CreateInstance(api_key);
>  ClientCoding := TMistralAIFactory.CreateInstance(Codestral_api_key);
>```

[Obtain the Codestral_api_key](https://console.mistral.ai/codestral)

To streamline the use of the API wrapper, the process for declaring units has been simplified. Regardless of the methods being utilized, you only need to reference the following two core units:
`MistralAI` and `MistralAI.Types`.

<br>

>[!TIP]
> To effectively use the examples in this tutorial, particularly when working with asynchronous methods, it is recommended to define the client interfaces with the broadest possible scope. For optimal implementation, these clients should be declared in the application's `OnCreate` method.
>

<br>

## Getting started with Mistral AI API

Mistral AI’s API lets you plug the  models into your apps and production pipelines in just a few lines of code. It’s currently accessible via La Plateforme—just make sure you’ve activated billing on your account so your API keys will work. Within moments of enabling payments, you’ll be able to call our chat endpoint:

- [Synchronous code example](#synchronous-code-example)
- [Asynchronous code example](#asynchronous-code-example)

<br>

### Synchronous code example

```Pascal
//uses MistralAI, MistralAI.Types;

  var API_Key := 'MISTRAL_API_KEY';
  var MyClient := TMistralAIFactory.CreateInstance(API_KEY);

  //Synchronous example
  var Chat := MyClient.Chat.Create(
    procedure (Params: TChatParams)
    begin
      Params
        .Model('mistral-tiny')
        .Messages([Payload.User('Explain to me what joual is for Quebecers.')])
        .MaxTokens(1024);
    end);
  try
    for var Item in Chat.Choices do
      Memo1.Text := Item.Message.Content[0].Text;
  finally
    Chat.Free;
  end;
```

<br>

___

### Asynchronous code example

```Pascal
var MyClient: IMistralAI;

procedure TForm1.Test;
begin
  var API_Key := 'MISTRAL_API_KEY';
  MyClient := TMistralAIFactory.CreateInstance(API_KEY);

  //Asynchronous example
  MyClient.Chat.AsyncCreate(
    procedure (Params: TChatParams)
    begin
      Params
        .Model('mistral-tiny')
        .Messages([Payload.User('Explain to me what joual is for Quebecers.')])
        .MaxTokens(1024);
    end,
    function : TAsynChat
    begin
      Result.OnStart :=
        procedure (Sender: TObject)
        begin
          Memo1.Lines.Text := 'Please wait...';
        end;

      Result.OnSuccess :=
        procedure (Sender: TObject; Value: TChat)
        begin
          for var Item in Value.Choices do
            Memo1.Lines.Text := Item.Message.Content[0].Text;
        end;

      Result.OnError :=
        procedure (Sender: TObject; Error: string)
        begin
          Memo1.Lines.Text := Error;
        end;
    end);
end;
```

<br>

___

# Documentation Overview

Comprehensive Project Documentation Reference

#### 1. Model Discovery & Metadata
-  Discovery / Evolution 
    - [Changelog](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Changelog.md) 
    - [Models](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Models.md#model-management-api-v1models) 

#### 2. Generation / Completions
- Standard Completions 
   - [Chat completions](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#chat-api-v1chatcompletions) 
   - [Agents completions](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsCompletions.md#agents-completions-v1agentscompletions)

- Specialized Completions  
   - [Fill-in-the-middle](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/FIM.md#fill-in-the-middle-v1fimcompletions) 

- Experimental Conversations 
   - [(beta) Conversations](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Conversations.md#conversations-api-v1conversations) 

#### 3. Orchestration & Agents
- [(beta) Agents API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md#agents--connectors-v1agents) 
- [Agents completions](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsCompletions.md#agents-completions-v1agentscompletions) 

#### 4. Data & Persistence
- Files and Libraries <br>
  - [Files](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#files-v1files) <br>
  - [(beta) Libraries API to create and manage libraries](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesMain.md#libraries-api---main) 
  - [(beta) Libraries API - manage documents in a library](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesDocuments.md#libraries-api---documents) 
  - [(beta) Libraries API - manage access to a library](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesAccess.md#libraries-api---access) 
- Vector Representations
   - [Embeddings](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Embeddings.md#embeddings-v1embeddings)

#### 5. Safety & Filtering
- [Moderation](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Classifiers.md#moderation)

#### 6. Customization / Tuning
- [Fine-tuning](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/FineTuning.md#fine-tuning-api--v1fine_tuningjobs)
- [Batch](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Batch.md#batch-inference-v1batchjobs)

#### 7. Multimodal / Non-text Input
- [OCR](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Ocr.md#ocr-api-v1ocr)
- [Audio](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AudioTranscriptions.md#audio)


<br>

___

# TIPS for using the tutorial effectively

- [Strategies for quickly using the code examples](#strategies-for-quickly-using-the-code-examples)
- [Use file2knowledge](#use-file2knowledge)

<br>

## Strategies for quickly using the code examples

To streamline the implementation of the code examples provided in this tutorial, two support units have been included in the source code: `MistralAI.Tutorial.VCL` and `MistralAI.Tutorial.FMX` Based on the platform selected for testing the provided examples, you will need to initialize either the `TVCLTutorialHub` or `TFMXTutorialHub` class within the application's OnCreate event, as illustrated below:

<br>

>[!IMPORTANT]
>In this repository, you will find in the [`sample`](https://github.com/MaxiDonkey/DelphiMistralAI/tree/main/sample) folder two ***ZIP archives***, each containing a template to easily test all the code examples provided in this tutorial. 
>Extract the `VCL` or `FMX` version depending on your target platform for testing. 
>Next, add the path to the DelphiMistralAI library in your project’s options, then copy and paste the code examples for immediate execution. 
>
>These two archives have been designed to fully leverage the TutorialHub middleware and enable rapid upskilling with DelphiMistralAI.

- [**`VCL`**](https://github.com/MaxiDonkey/DelphiMistralAI/tree/main/sample) support with TutorialHUB: ***TestMistralAI_VCL.zip***

- [**`FMX`**](https://github.com/MaxiDonkey/DelphiMistralAI/tree/main/sample) support with TutorialHUB: ***TestMistralAI_FMX.zip***

<br>

## [Use file2knowledge](https://github.com/MaxiDonkey/file2knowledge)

This [project](https://github.com/MaxiDonkey/file2knowledge), built with `DelphiGenAI` , allows you to consult `MistralAI` documentation and code in order to streamline and accelerate your upskilling.

![Preview](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/sample/File2knowledge.png?raw=true "Preview")

<br>

___

# DelphiMistralAI functional coverage

Below, the table succinctly summarizes all MistralAI endpoints supported by the DelphiMistralAI.

| Description | End point | supported | 
|--- |--- |:---: | 
|[Chat Completion API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#chat-api-v1chatcompletions) | v1/chat/completions | <div align="center"><span style="color: green;">●</span></div> |
|[Fill-in-the-middle API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/FIM.md#fill-in-the-middle-v1fimcompletions) |v1/fim/completions | <div align="center"><span style="color: green;">●</span></div> |
|[Agents completions API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsCompletions.md#agents-completions-v1agentscompletions) | v1/agents/completions | <div align="center"><span style="color: green;">●</span></div> |
|[Embeddings API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Embeddings.md#embeddings-v1embeddings) | v1/embeddings | <div align="center"><span style="color: green;">●</span></div> |
|[Classifiers API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Classifiers.md#moderation) | v1/moderations | <div align="center"><span style="color: green;">●</span></div> |
| | v1/chat/moderations | <div align="center"><span style="color: green;">●</span></div> |
| | v1/classifications |  |
| | v1/chat/classifications |  |
|[Files API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Files.md#files-v1files) | v1/files | <div align="center"><span style="color: green;">●</span></div> |
|[Fine-tuning API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/FineTuning.md#fine-tuning-api--v1fine_tuningjobs) | v1/fine_tuning/jobs | <div align="center"><span style="color: green;">●</span></div> |
|[Model Management API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Models.md#model-management-api-v1models) | v1/models | <div align="center"><span style="color: green;">●</span></div> |
| | v1/fine_tuning/models | <div align="center"><span style="color: green;">●</span></div> |
|[Batch API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Batch.md#batch-inference-v1batchjobs) | v1/batch/jobs | <div align="center"><span style="color: green;">●</span></div> |
|[OCR API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Ocr.md#ocr-api-v1ocr) | v1/ocr | <div align="center"><span style="color: green;">●</span></div> |
|[(beta) Agents API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md#agents--connectors-v1agents) | v1/agents | <div align="center"><span style="color: green;">●</span></div> |
|[(beta) Conversations API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Conversations.md#conversations-api-v1conversations) | v1/conversations | <div align="center"><span style="color: green;">●</span></div> |
|[(beta) Libraries API to create and manage libraries](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesMain.md#libraries-api---main) | v1/libraries| <div align="center"><span style="color: green;">●</span></div> |
|[(beta) Libraries API - manage documents in a library](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesDocuments.md#libraries-api---documents)| v1/libraries/{library_id}/documents | <div align="center"><span style="color: green;">●</span></div> |
|[(beta) Libraries API - manage access to a library](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/LibrariesAccess.md#libraries-api---access) | v1/libraries/{library_id}/share | <div align="center"><span style="color: green;">●</span></div> |
|[Audio transcriptions](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AudioTranscriptions.md#audio) | v1/audio/transcriptions | <div align="center"><span style="color: green;">●</span></div> |

<br>

___

# Conversations vs. Chat Completions

The `v1/conversations` API is the new core API, designed as an agentic primitive that combines the simplicity of chat completions with the power of action execution. It natively includes several built‑in tools:
- Reasoning
- Web search
- Code interpreter
- Image generation
- Document library

With these integrated capabilities, you can build more autonomous, agent‑oriented applications that not only generate text but also interact with their environment.

The `v1/conversations` endpoint is intended to gradually replace `v1/chat/completions`, as it embodies a synthesis of current best practices in AI—especially for those looking to adopt an agentic approach.

To help you get up to speed on both endpoints, the two following documents provide detailed documentation, complete with numerous request examples and use cases:
- [v1/chat/completions](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#chat-api-v1chatcompletions)
- [v1/conversations](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Conversations.md#conversations-api-v1conversations)

>[!NOTE]
>If you're a new user, we recommend using the `v1/conversations` API.

<br>

## Functional differences between the three endpoints

|Capabilities | [Chat Completions API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#chat-api-v1chatcompletions) | [Conversations API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/Conversations.md#conversations-api-v1conversations) | [Agents & connectors API](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md#agents--connectors-v1agents) |
| :---: |:---: | :---: | :---: |
|Text generation | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> | |
| Vision | <div align="center"><span style="color: green;">●</span></div> | | |
| Audio  | <div align="center"><span style="color: green;">●</span></div> |  |  |
| Function calling | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |  |
| Structured Outputs | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |
| Reasoning | <div align="center"><span style="color: green;">●</span></div>  | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |
| Web search |  | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |
| Image generation | | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |
| Document library |  | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |
| Code interpreter |  | <div align="center"><span style="color: green;">●</span></div> | <div align="center"><span style="color: green;">●</span></div> |

>[!IMPORTANT]
> Note: Agent and Connector work in conjunction with `v1/conversations`, meaning they are simply tools invoked within the context of those conversations.

<br>

___

# Tips and tricks

<br>

### How to prevent an error when closing an application while requests are still in progress?

Starting from version ***1.3.0 of DelphiMistralAI***, the `MistralAI.Monitoring` unit is **responsible for monitoring ongoing HTTP requests.**

The `Monitoring` interface is accessible by including the `MistralAI.Monitoring` unit in the `uses` clause. <br>
Alternatively, you can access it via the `HttpMonitoring` function, declared in the `MistralAI` unit.

**Usage Example**

```Pascal
//uses MistralAI;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not HttpMonitoring.IsBusy;
  if not CanClose then
    MessageDLG(
      'Requests are still in progress. Please wait for them to complete before closing the application."',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

```

___

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

<br>

___

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

<br>