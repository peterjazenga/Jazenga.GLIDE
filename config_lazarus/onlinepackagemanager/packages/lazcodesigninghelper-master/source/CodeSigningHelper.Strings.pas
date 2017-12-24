{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Texts used by Codesigning package.)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-16  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Strings;

interface

resourcestring
   rsCodeSigningErrorDoProjectClose = 'An exception occured handling codesigning specific data in DoProjectClose(%s)';
   rsCodeSigningErrorDoProjectOpened = 'An exception occured handling codesigning specific data in DoProjectOpened(%s)';
   rsCodeSigningErrorGetPrimaryConfigPath = 'Unable to get LazarusIDE.GetPrimaryConfigPath:';
   rsCodeSigningErrorIDEMacrosSubstituteMacrosFailed = 'Unable to get target filename for codesigning, IDEMacros.SubstituteMacros(%s) failed!';
   rsCodeSigningErrorLazarusIDEActiveProjectMainFileFilename = 'Unable to get LazarusIDE.ActiveProject.MainFile.Filename:';
   rsCodeSigningErrorMissingSigningExecutable = 'Unable to find signing executable %s!';
   rsCodeSigningErrorTargetOSAppleCodeSign = 'Apple CodeSign is not designed to sign files for the target platform "%s"!';
   rsCodeSigningErrorTargetOSMicrosoftSignTool = 'Microsoft SignTool is not designed to sign files for the target platform "%s"!';
   rsCodeSigningErrorWritingConfig = 'There was an issue trying to save CodeSigningHelper settings to %s:'#13#10'%s';
   rsCodeSigningMenuItemSign = 'Attach code signature to executable';
   rsCodeSigningMenuItemSignBundle = 'Attach code signature to bundle';
   rsCodeSigningMenuItemSignOther = 'Attach code signature to other file...';
   rsCodeSigningMenuItemSignOtherGlobal = 'Attach code signature with global options to other file...';
   rsCodeSigningMenuItemVerify = 'Verify code signature';
   rsCodeSigningMenuItemVerifyOther = 'Verify code signature of other file...';
   rsCodeSigningMenuName = 'Codesign Project';
   rsCodeSigningMessageCertificateSourceFile = 'Certificate file: %s';
   rsCodeSigningMessageCertificateSourceHash = 'Certificate SHA-1: %s';
   rsCodeSigningMessageCertificateSourceSubstring = 'Certificate substring: %s';
   rsCodeSigningMessageCrossSigningActive = 'Cross-signing active: %s';
   rsCodeSigningMessageCrossSigningFile = 'Cross-signing certificate: %s';
   rsCodeSigningMessageDescriptionText = 'Description text: %s';
   rsCodeSigningMessageDescriptionURL = 'Description URL: %s';
   rsCodeSigningMessageError = 'Unable to sign file %s';
   rsCodeSigningMessagePageHashingDefault = 'Page Hashing: default';
   rsCodeSigningMessagePageHashingNo = 'Page Hashing: no (/nph)';
   rsCodeSigningMessagePageHashingYes = 'Page Hashing: yes (/ph)';
   rsCodeSigningMessageProjectSpecificAlgoritms = 'Using project-specific algorithms...';
   rsCodeSigningMessageProjectSpecificCertificate = 'Using project-specific certificate...';
   rsCodeSigningMessageProjectSpecificFlags = 'Using project-specific flags...';
   rsCodeSigningMessageProjectSpecificTimestamping = 'Using project-specific timestamping...';
   rsCodeSigningMessageSuccess = 'Signed %s';
   rsCodeSigningMessageTimestampingActive = 'Timestamping active: %s';
   rsCodeSigningMessageTimestampingServer = 'Timestamping server: %s';
   rsCodeSigningMessageWindowsSystemComponentVerification = 'Windows System Component Verification: %s';
   rsCodeSigningOptionsAlgorithms = 'Algorithms';
   rsCodeSigningOptionsAutomation = 'Automation';
   rsCodeSigningOptionsBooleanHint = 'Global setting: %s';
   rsCodeSigningOptionsGroupName = 'Code Signing Options';
   rsCodeSigningOptionsMoreOptions = 'More options';
   rsCodeSigningOptionsPageHashingPh = 'Page hashing (/ph)';
   rsCodeSigningOptionsSigningKey = 'Signing Key';
   rsCodeSigningOptionsSigningToolExecutable = 'Signing tool executable';
   rsCodeSigningOptionsSignOnSuccessfulCompileOrBuild = 'Sign on successful compile or build';
   rsCodeSigningOptionsSignWithSHA1 = 'Sign with SHA-1';
   rsCodeSigningOptionsSignWithSHA256 = 'Sign with SHA-256';
   rsCodeSigningOptionsSpecifiyKeyAndCertificateAsPfxFile = 'Specifiy key and certificate as .pfx file:';
   rsCodeSigningOptionsSpecifiyKeyByPartOfSubject = 'Specifiy key by part of subject:';
   rsCodeSigningOptionsSpecifiyKeyInCertificateStoreByHash = 'Specifiy key in certificate store by hash:';
   rsCodeSigningOptionsTimestamping = 'Timestamping';
   rsCodeSigningOptionsTitle = 'Code Signing';
   rsCodeSigningOptionsTitleAppleCodeSign = 'Apple CodeSign';
   rsCodeSigningOptionsTitleJavaKeyTool = 'Java KeyTool';
   rsCodeSigningOptionsTitleMicrosoftSignTool = 'Microsoft SignTool';
   rsCodeSigningOptionsUseCrossSigningCertificate = 'Use cross-signing certificate:';
   rsCodeSigningOptionsUseTimestamping = 'Use timestamping';
   rsCodeSigningOptionsWindowsSystemComponentVerificationUw = 'Windows System Component Verification (/uw)';
   rsCodeSigningProjectOptionsGroupName = 'Code Signing Project Options';
   rsCodeSigningStatusBuildFailed = 'Build not successful, skipping signing.';
   rsCodeSigningStatusDeleteUnbundledExecutableFail = 'Unable to delete executable in old location at %s';
   rsCodeSigningStatusErrorCode = 'Error Code: %d';
   rsCodeSigningStatusExecutableToBundleCopiedFail = 'Unable to copy executable into bundle at %s';
   rsCodeSigningStatusExecutableToBundleCopiedGood = 'Copied executable into bundle at %s';
   rsCodeSigningStatusSignatureFolderRemovedFail = 'Unable to remove signature folder at %s';
   rsCodeSigningStatusSignatureFolderRemovedGood = 'Removed signature folder at %s';
   rsCodeSigningStatusSignatureRemovedFail = 'Unable to remove signature at %s';
   rsCodeSigningStatusSignatureRemovedGood = 'Removed signature at %s';
   rsCodeSigningStatusSigning = 'Codesigning executable...';
   rsCodeSigningStatusSymlinkRemovedFail = 'Unable to delete symlink or old executable at %s';
   rsCodeSigningViewCodeSign = 'Codesign Project';
   rsCodeSigningViewCodeVerify = 'Verify Project Signature';
   rsGnuPGSigningMenuItemSign = 'Create GnuPG signature for executable';
   rsGnuPGSigningMenuItemSignOther = 'Create GnuPG signature for other file...';
   rsGnuPGSigningMenuItemSignOtherGlobal = 'Create GnuPG signature with global options for other file...';
   rsGnuPGSigningMenuItemVerify = 'Verify GnuPG signature of executable';
   rsGnuPGSigningMenuItemVerifyOther = 'Verify GnuPG signature of other file...';
   rsGnuPGSigningMenuName = 'GnuPG-sign Project';
   rsGnuPGSigningOptionsUseNonDefaultSigningKey =  'Use non-default signing key:';
   rsGnuPGSigningStatusSigning = 'Signing executable with GnuPG...';
   rsGnuPGSigningViewSign = 'GnuPG-sign Project';
   rsGnuPGSigningViewVerify = 'Verify GnuPG Signature';

implementation

end.
