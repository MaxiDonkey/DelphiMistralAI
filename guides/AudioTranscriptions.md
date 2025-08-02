# Audio

- [Introduction](#introduction)
- [Transcription](#transcription)
- [Chat with Audio](#chat-with-audio)

<br>

___

## Introduction

These APIs expose two core audio capabilities: **multimodal conversation** (chat with audio) and **optimized transcription**.

### 1. Audio chat:

You can send an audio file—either embedded directly or referenced via a URL (***local, remote, or through a signed upload URL***)—and the model will respond as in a normal conversation. The audio is treated as input just like text: it’s analyzed, meaning is extracted, and you can ask follow-ups such as “what’s in this file?” or continue an interaction. There are two model variants depending on latency/quality needs (`Voxtral Small` and `Voxtral Mini`), with a practical upper bound of roughly 20 minutes per recording in this mode.

### 2. Transcription:

A dedicated endpoint converts audio into text, with automatic language detection or the option to specify the language if known to improve accuracy. It also supports temporal granularity (e.g., segmentation) so you can know not just what was said but when. Audio can be provided directly, via a public URL, or by uploading first and using a signed URL. The transcription-optimized model has a slightly tighter practical limit (≈15 minutes) and returns structured text, optionally with time markers per segment.

### 3. File handling:

To avoid embedding large binaries directly (like base64), you can upload audio files to the service, retrieve a temporary signed URL, and then reuse that URL for chat or transcription calls. This cleanly separates transport of the raw audio from the business logic.

### 4. Fine-grained options:

- ***Language:*** auto-detected or manually specified when known.

- ***Timestamps:*** useful for post-processing, alignment, or slicing based on when content was spoken.

- ***Segmentation:*** receive the transcript broken into segments with start/end times—valuable for navigation or indexing.

### 5. Constraints and best practices:

- Max duration per call varies by mode: about 20 minutes for audio chat, 15 minutes for transcription.

- For longer recordings, split into chunks—being mindful of potential context loss and the need to stitch transcripts together cleanly.

- Another lever is adjusting playback speed to feed audio faster, which requires recalibrating timestamps if the speed was modified.

### 6. Typical use cases:

- Extracting and summarizing meetings or interviews.

- Conversational interfaces driven by speech instead of typing.

- Indexed search within recordings using time anchors.

- Audio+NLP pipelines (language detection, segmentation, automated note-taking, triggerable actions).

<br>

___

## Transcription

The Transcription API converts audio into time-aligned text with automatic language detection. It hides all input mechanics: whether the audio comes as base64, a public URL, or a signed URL from an upload, the wrapper detects and normalizes it transparently—just supply your audio source and get back a structured transcript with optional timestamps. Designed for short-to-medium recordings (≈15 minutes per request), it’s a low-friction building block for meeting notes, searchable audio archives, voice-driven interfaces, and downstream NLP.

Refer to [official documentation](https://docs.mistral.ai/capabilities/audio/)

### Exemple

```Pascal
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;
  
  TutorialHub.JSONRequestClear;
  var SignedUrl := 'https://signed_url';
  var Url := 'https://docs.mistral.ai/audio/obama.mp3';
  var FilePath := 'C:\my_path\my_audio_file.mp3';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Audio.AsyncAwaitTranscription(
    procedure (Params: TAudioTranscriptionParams)
    begin
      Params.Model('voxtral-mini-latest');
      Params.FileUrl(SignedUrl);  //or Params.FileUrl(Url); or Params.FileUrl(FilePath);
      Params.Language('fr');
      TutorialHub.JSONRequest := 'multipart';
    end);

  promise
    .&Then<TAudioTranscription>(
      function (Value: TAudioTranscription): TAudioTranscription
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
//  Client.Audio.AsyncTranscription(
//    procedure (Params: TAudioTranscriptionParams)
//    begin
//      Params.Model('voxtral-mini-latest');
//      Params.FileUrl(SignedUrl); //or Params.FileUrl(Url); or Params.FileUrl(FilePath);
//      Params.Language('fr');
//      TutorialHub.JSONRequest := 'multipart';
//    end,
//    function : TAsyncAudioTranscription
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Audio.Transcription(
//    procedure (Params: TAudioTranscriptionParams)
//    begin
//      Params.Model('voxtral-mini-latest');
//      Params.FileUrl(SignedUrl); //or Params.FileUrl(Url); or Params.FileUrl(FilePath);
//      Params.Language('fr');
//      TutorialHub.JSONRequest := 'multipart';
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Chat with Audio

Refer to Input [Audio for Chat](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#input-audio-for-chat) section.