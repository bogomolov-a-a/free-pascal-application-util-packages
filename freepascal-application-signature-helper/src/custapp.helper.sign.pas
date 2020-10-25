{Helper implementation for CustApp}
unit CustApp.Helper.Sign;

{$mode objfpc}{$H+}

interface

uses CustApp  {$IFDEF MSWINDOWS}, jwaWinCrypt{$ENDIF}, SysUtils;

type

  { TCustAppSignatureCheckHelper }

  TCustAppSignatureCheckHelper = class helper for TCustomApplication
  {$IFDEF MSWINDOWS}
    strict  private
    procedure CheckSigningCertificate(SerialNumber: string; SubjectName: string;
      Thumbprint: string);
    procedure CheckTrustedStatus(TrustedResultStatus: longint);
    function ReadCertContext(hMsg: HCRYPTMSG; dwEncoding: DWORD;
      hStore: HCERTSTORE): PCCERT_CONTEXT;
    procedure CheckSerialNumber(CertContextInfo: CERT_INFO; SerialNumber: string);
    function CreateSerialNumber(SerialNumberBlob: CRYPT_INTEGER_BLOB): string;
    procedure CheckSubjectName(CertInfo: CERT_INFO; SubjectName: string);
    procedure CheckHashCertifacate(CertContext: PCCERT_CONTEXT; Thumbprint: string);
    function CreateComputedHash(ComputedHashBytes: TBytes): string;
  {$ELSE}
  {$ENDIF}
  public
    procedure CheckCodeSignature(SerialNumber: string; SubjectName: string;
      Thumbprint: string);
  end;

  ESignatureVerificationException = class(Exception);

const
  W_APPLICATION_CODE_CAN_MODIFIED =
    'Application code may be modified by another side and won''t match with original';

resourcestring

  E_CANT_GET_SIGNER_INFO = 'Can''t get signer info data. Cause: %s';
  E_CANT_GET_CERTIFICATE_TABLES = 'Can''t get certificate tables. Cause: %s';

  E_SIGNATURE_NOT_FOUND = 'Not found signature! .' + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_NOT_TRUSTED = 'Found signature,but not trusted it.' +
    W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_EXPIRED =
    'Found signature,but one of certificates expired.' + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_ROOT_CERTIFICATE_UNTRUSTED =
    'Found signature,but root certificate is untrusted.' +
    W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_MALFORMED =
    'Found signature,but  certificate is malformed.' + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_PARENT_CERTIFICATE_CANT_ISSUE_CERTIFICATE =
    'Found signature,but parent in fact did not issue that child certificate.' +
    W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_PATH_LENGTH_VIOLATED =
    'Found signature, but a path length constraint in the certification chain has been violated.'
    + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_CHAIN_CORRUPTED =
    'Found signature,but can''t verifed certificate chain.' +
    W_APPLICATION_CODE_CAN_MODIFIED;
  E_UNKNOWN_SIGNATURE_VERIFING_ERROR =
    'Unknown error occured during signature verification. ' +
    LineEnding + 'Reason %s.' + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_WRONG_SERIAL_NUMBER =
    'Wrong serial number %s.' + W_APPLICATION_CODE_CAN_MODIFIED;
  E_SIGNATURE_CERTIFICATE_HASH_CANT_COMPUTED = 'Hash can''t be computed.%s';
  E_SIGNATURE_CERTIFICATE_WRONG = 'Computed hash %s wrong!';
  E_SIGNATURE_CERTIFICATE_SUBJECT_NAME_WRONG = 'Wrong subject name: %s';

implementation
{$IFDEF windows}
uses Windows;

//import WinVerifyTrust from  WinTrust.dll
const
  wintrust = 'WinTrust.dll';
  WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID = '{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}';

  WTD_CHOICE_FILE = $0001;
  WTD_REVOKE_NONE = $0000;

  WTD_UI_NONE = $0002;

  WTD_DISABLE_MD2_MD4 = $2000;
  WTD_REVOCATION_CHECK_CHAIN = $40;

  WTD_STATEACTION_VERIFY = $0001;
  WTD_STATEACTION_CLOSE = $0002;

type
  PWinTrustFileInfo = ^TWinTrustFileInfo;

  TWinTrustFileInfo = record
    cbStruct: DWORD;                    // = sizeof(WINTRUST_FILE_INFO)
    pcwszFilePath: PWideChar;           // required, file name to be verified
    hFile: THandle;                     // optional, open handle to pcwszFilePath
    pgKnownSubject: PGUID;              // optional: fill if the subject type is known
  end;

  PWinTrustData = ^TWinTrustData;

  TWinTrustData = record
    cbStruct: DWORD;
    pPolicyCallbackData: Pointer;
    pSIPClientData: Pointer;
    dwUIChoice: DWORD;
    fdwRevocationChecks: DWORD;
    dwUnionChoice: DWORD;
    pFile: PWinTrustFileInfo;
    dwStateAction: DWORD;
    hWVTStateData: THandle;
    pwszURLReference: PWideChar;
    dwProvFlags: DWORD;
    dwUIContext: DWORD;
  end;

function WinVerifyTrust(hwnd: HWND; const ActionID: TGUID; ActionData: Pointer): longint;
  stdcall; external wintrust;
{import from crypto32.dll}
const
  {default hash algorithm used for hash computation.}
  BCRYPT_SHA1_ALGORITHM = 'SHA1'#0;

function CryptHashCertificate2(Algid: LPWSTR; dwFlags: DWORD;
  pvReserved: Pointer; pbEncoded: LPBYTE; cbEncoded: DWORD;
  pbComputedHash: LPBYTE; var pcbComputedHash: DWORD): BOOL; stdcall;
  external 'crypt32.dll' Name 'CryptHashCertificate2';
{$ENDIF}
{ TCustAppSignatureCheckHelper }

procedure TCustAppSignatureCheckHelper.CheckCodeSignature(SerialNumber: string;
  SubjectName: string; Thumbprint: string);
{$IFDEF windows}
var
  FileInfo: TWinTrustFileInfo;
  TrustData: TWinTrustData;
  Status: integer;
{$ENDIF}
begin
  {$IFDEF windows}
  try
    // Verify that the exe is signed and the checksum matches
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    FileInfo.cbStruct := sizeof(FileInfo);
    FileInfo.pcwszFilePath := PWideChar(WideString(self.ExeName));
    FillChar(TrustData, SizeOf(TrustData), 0);
    TrustData.cbStruct := sizeof(TrustData);
    TrustData.dwUIChoice := WTD_UI_NONE;
    TrustData.fdwRevocationChecks := WTD_REVOKE_NONE;
    TrustData.dwUnionChoice := WTD_CHOICE_FILE;
    TrustData.dwStateAction := WTD_STATEACTION_VERIFY;
    TrustData.pFile := @FileInfo;
    TrustData.dwProvFlags := WTD_DISABLE_MD2_MD4 or WTD_REVOCATION_CHECK_CHAIN;
    Status := WinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2,
      @TrustData);
    CheckTrustedStatus(Status);
  finally
    TrustData.dwStateAction := WTD_STATEACTION_CLOSE;
    WinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2,
      @TrustData)
  end;
  {check certificate data(such as serial number )}
  CheckSigningCertificate(SerialNumber, SubjectName, Thumbprint);
  {$ENDIF}
end;
{$IFDEF windows}
procedure TCustAppSignatureCheckHelper.CheckTrustedStatus(
  TrustedResultStatus: longint);
begin
  case (TrustedResultStatus) of
    ERROR_SUCCESS:
    begin
      exit;
    end;
    TRUST_E_NOSIGNATURE:
    begin
      raise ESignatureVerificationException.Create(E_SIGNATURE_NOT_FOUND);
    end;
    TRUST_E_SUBJECT_NOT_TRUSTED:
    begin
      raise ESignatureVerificationException.Create(E_SIGNATURE_NOT_TRUSTED);
    end;
    CERT_E_EXPIRED:
    begin
      raise ESignatureVerificationException.Create(E_SIGNATURE_CERTIFICATE_EXPIRED);
    end;
    CERT_E_UNTRUSTEDROOT:
    begin
      raise ESignatureVerificationException.Create(
        E_SIGNATURE_ROOT_CERTIFICATE_UNTRUSTED);
    end;
    CERT_E_MALFORMED:
    begin
      raise ESignatureVerificationException.Create(E_SIGNATURE_CERTIFICATE_MALFORMED);
    end;
    CERT_E_ISSUERCHAINING:
    begin
      raise ESignatureVerificationException.Create(
        E_SIGNATURE_PARENT_CERTIFICATE_CANT_ISSUE_CERTIFICATE);
    end;
    CERT_E_PATHLENCONST:
    begin
      raise ESignatureVerificationException.Create(
        E_SIGNATURE_CERTIFICATE_PATH_LENGTH_VIOLATED);
    end;
    CERT_E_CHAINING:
    begin
      raise ESignatureVerificationException.Create(
        E_SIGNATURE_CERTIFICATE_CHAIN_CORRUPTED);
    end
    else
    begin
      raise ESignatureVerificationException.CreateFmt(
        E_UNKNOWN_SIGNATURE_VERIFING_ERROR, [IntToHex(GetLastError)]);
    end;
  end;
end;

procedure TCustAppSignatureCheckHelper.CheckSigningCertificate(SerialNumber: string;
  SubjectName: string; Thumbprint: string);
var
  CertCount: DWORD;
  CertName: ansistring;
  CertNameLen: DWORD;
  dwEncoding, dwContentType, dwFormatType: DWORD;
  hStore: HCERTSTORE;
  hMsg: HCRYPTMSG;
  CertContext: PCCERT_CONTEXT;
  CertInfo: CERT_INFO;
begin
  if not CryptQueryObject(CERT_QUERY_OBJECT_FILE, PWideChar(WideString(self.ExeName)),
    CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED, CERT_QUERY_FORMAT_FLAG_BINARY,
    0, @dwEncoding, @dwContentType, @dwFormatType, @hStore, @hMsg, nil) then
  begin
    raise ESignatureVerificationException.CreateFmt(E_CANT_GET_CERTIFICATE_TABLES,
      [IntToHex(GetLastError)]);
  end;
  try
    CertContext := ReadCertContext(hMsg, dwEncoding, hStore);
    CertInfo := CertContext^.pCertInfo^;
    CheckSerialNumber(CertInfo, SerialNumber);
    CheckSubjectName(CertInfo, SubjectName);
    CheckHashCertifacate(CertContext, Thumbprint);
  finally
    //Clean Up
    CertFreeCertificateContext(CertContext);
    if hStore <> nil then
      CertCloseStore(hStore, 0);
    if hMsg <> nil then
      CryptMsgClose(hMsg);
  end;
end;

function TCustAppSignatureCheckHelper.ReadCertContext(hMsg: HCRYPTMSG;
  dwEncoding: DWORD; hStore: HCERTSTORE): PCCERT_CONTEXT;
var
  SignerInfoSize: DWORD;
  SignerInfo: PCMSG_SIGNER_INFO;
  CertInfo: CERT_INFO;
begin
  SignerInfo := PCMSG_SIGNER_INFO(LocalAlloc(LPTR, SignerInfoSize));
  try
    if not CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, nil, SignerInfoSize) then
    begin
      raise  ESignatureVerificationException.CreateFmt(E_CANT_GET_SIGNER_INFO,
        [IntToHex(GetLastError)]);
    end;
    if not CryptMsgGetParam(hMsg, CMSG_SIGNER_INFO_PARAM, 0, SignerInfo,
      SignerInfoSize) then
    begin
      raise ESignatureVerificationException.CreateFmt(E_CANT_GET_SIGNER_INFO,
        [IntToHex(GetLastError)]);
    end;
    CertInfo.SerialNumber := SignerInfo^.SerialNumber;
    CertInfo.Issuer := SignerInfo^.Issuer;
    Result := CertFindCertificateInStore(hStore, dwEncoding, 0,
      CERT_FIND_SUBJECT_CERT, @CertInfo, nil);
  finally
    LocalFree(HLocal(SignerInfo));
  end;
end;

procedure TCustAppSignatureCheckHelper.CheckSerialNumber(CertContextInfo: CERT_INFO;
  SerialNumber: string);
var
  GivenSerialNumber: string;
begin
  GivenSerialNumber := CreateSerialNumber(CertContextInfo.SerialNumber);
  if GivenSerialNumber.ToUpper <> SerialNumber.ToUpper then
    raise ESignatureVerificationException.CreateFmt(
      E_SIGNATURE_CERTIFICATE_WRONG_SERIAL_NUMBER, [GivenSerialNumber]);
end;

function TCustAppSignatureCheckHelper.CreateSerialNumber(
  SerialNumberBlob: CRYPT_INTEGER_BLOB): string;
var
  i, Count: integer;
begin
  Result := '';
  Count := SerialNumberBlob.cbData;
  for i := 0 to Count - 1 do
    Result := Result + IntToHex(SerialNumberBlob.pbData[Count - (i + 1)]);
end;

procedure TCustAppSignatureCheckHelper.CheckSubjectName(CertInfo: CERT_INFO;
  SubjectName: string);
var
  Count, Result: Dword;
  GivenSubjectName: string;
begin
  GivenSubjectName := '';
  Count := CertNameToStr(X509_ASN_ENCODING, @(CertInfo.Subject),
    CERT_X500_NAME_STR, nil, 0);
  SetLength(GivenSubjectName, Count);
  Result := CertNameToStr(X509_ASN_ENCODING, @(CertInfo.Subject),
    CERT_X500_NAME_STR, @GivenSubjectName[1], Count);
  {remove \0 terminator.}
  GivenSubjectName := GivenSubjectName.Substring(0, GivenSubjectName.Length - 1);
  if GivenSubjectName <> SubjectName then
    raise ESignatureVerificationException.CreateFmt(
      E_SIGNATURE_CERTIFICATE_SUBJECT_NAME_WRONG, [GivenSubjectName]);
end;

procedure TCustAppSignatureCheckHelper.CheckHashCertifacate(
  CertContext: PCCERT_CONTEXT;
  Thumbprint: string);
var
  ComputedHashBytes: TBytes;
  ComputedHashSize: DWORD;
  ComputedHash: string;
begin
  ComputedHash := '';
  if not CryptHashCertificate2(BCRYPT_SHA1_ALGORITHM, 0, nil,
    CertContext^.pbCertEncoded, CertContext^.cbCertEncoded, nil, ComputedHashSize) then
  begin
    raise ESignatureVerificationException.CreateFmt(
      E_SIGNATURE_CERTIFICATE_HASH_CANT_COMPUTED, [IntToHex(GetLastError)]);
  end;
  SetLength(ComputedHashBytes, ComputedHashSize);
  if not CryptHashCertificate2(BCRYPT_SHA1_ALGORITHM, 0, nil,
    CertContext^.pbCertEncoded, CertContext^.cbCertEncoded,
    @ComputedHashBytes[0], ComputedHashSize) then
  begin
    raise ESignatureVerificationException.CreateFmt(
      E_SIGNATURE_CERTIFICATE_HASH_CANT_COMPUTED, [IntToHex(GetLastError)]);
  end;
  ComputedHash := CreateComputedHash(ComputedHashBytes);
  if ComputedHash.ToUpper <> Thumbprint.ToUpper then
    raise ESignatureVerificationException.CreateFmt(E_SIGNATURE_CERTIFICATE_WRONG,
      [ComputedHash]);
end;

function TCustAppSignatureCheckHelper.CreateComputedHash(
  ComputedHashBytes: TBytes): string;
var
  i, Count: integer;
begin
  Result := '';
  Count := Length(ComputedHashBytes);
  for i := 0 to Count - 1 do
    Result := Result + IntToHex(ComputedHashBytes[i]);
end;
{$ENDIF}

end.
