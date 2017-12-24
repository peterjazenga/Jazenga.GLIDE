unit unite_messages;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}


uses fonctions_string
{$IFDEF VERSIONS}
     ,fonctions_version ;

const
  gVer_unite_messages : T_Version= ( Component : 'Constantes messages' ; FileUnit : 'unite_messages' ;
                        	     Owner : 'Matthieu Giroux' ;
                        	     Comment : 'Constantes et variables messages.' ;
                        	     BugsStory : 'Version 1.0.6.0 : db messages.' + CST_ENDOFLINE
                                               + 'Version 1.0.5.0 : NetUpdate messages.' + CST_ENDOFLINE
                                               + 'Version 1.0.4.1 : Menu Toolbar messages.' + CST_ENDOFLINE
                                               + 'Version 1.0.4.0 : Message d''erreur de sauvegarde ini.' + CST_ENDOFLINE
                                               + 'Version 1.0.3.3 : Message GS_MC_ERREUR_CONNEXION.' + CST_ENDOFLINE
                                               + 'Version 1.0.3.2 : Modifs GS_MC_VALEUR_UTILISEE et GS_MC_VALEURS_UTILISEES, ajout de GS_MC_DETAILS_TECHNIQUES.' + CST_ENDOFLINE
                                               + 'Version 1.0.3.1 : Constante message Form Dico.' + CST_ENDOFLINE
                                               + 'Version 1.0.3.0 : Constantes INI.' + CST_ENDOFLINE
                                               + 'Version 1.0.2.0 : Plus de messages dans l''unité.' + CST_ENDOFLINE
                                               + 'Version 1.0.1.0 : Plus de messages dans l''unité.' + CST_ENDOFLINE
                                               + 'Version 1.0.0.0 : Gestion des messages des fenêtres.';
                        	     UnitType : 1 ;
                        	     Major : 1 ; Minor : 0 ; Release : 6 ; Build : 0 )

{$ENDIF};

// COmposants
const
  CST_RESSOURCENAV = 'EXTNAV' ;
  CST_RESSOURCENAVMOVE = 'MOVE' ;
  CST_RESSOURCENAVBOOKMARK = 'BOOKMARK' ;
  CST_HC_SUPPRIMER        = 0 ;
  CST_PALETTE_COMPOSANTS_INVISIBLE = 'ExtInvisible' ;
  CST_PALETTE_COMPOSANTS_DB = 'ExtDB' ;
  CST_PALETTE_COMPOSANTS    = 'ExtCtrls' ;
  CST_PALETTE_BOUTONS    = 'FWButtons' ;
  CST_Avant_Fichier = 'MG_';
  CST_ARG           = '@ARG' ;
  CST_ASYNCHRONE_TIMEOUT_DEFAUT = 30 ;
  CST_CONNECTION_TIMEOUT_DEFAUT : Integer = 15 ;
  CST_ASYNCHRONE_NB_ENREGISTREMENTS : Integer = 300 ;
  CST_CONNECTION_TIMEOUT = 'Connection TimeOut' ;
  CST_FORM_ONCLOSE = 'OnClose' ;
 {$IFDEF EADO}
  CST_Set_KEYSET = 'Set Keyset' ;
  CST_MODE_ASYNCHRONE = 'Mode Asynchrone' ;
  CST_ACCES_DIRECT_SERVEUR = 'Accès directs Serveur' ;
  CST_MODE_CONNEXION_ASYNCHRONE = 'Connection Asynchrone' ;
  CST_MODE_ASYNCHRONE_NB_ENREGISTREMENTS = 'Mode Asynchrone Enregistrements' ;
  CST_MODE_ASYNCHRONE_TIMEOUT = 'Mode Asynchrone TimeOut' ;
{$ENDIF}



resourcestring
  gs_TestOk  = 'Test OK' ;
  gs_TestBad  = 'Error' ;

  // Paquet extcore

  GS_ECRITURE_IMPOSSIBLE_AVEC_ATTR = 'Impossible d''écrire sur le fichier @ARG avec l''attribut de fichier @ARG.' ;
  GS_ECRITURE_IMPOSSIBLE = 'Impossible d''écrire sur le fichier @ARG.' ;
  GS_DETAILS_TECHNIQUES = 'Détails techniques : ' ;
  GS_TOOLBARMENU_Personnaliser = 'Personnaliser' ;

  GS_OF_DATASET = 'du Dataset ';

  GS_SOFT_IMAGE_NOT_FOUND   ='Image @ARG non tnouvée'+CST_ENDOFLINE +
                             'Veuillez copier le répertoire ''Images'' dans le répertoire de votre exécutable.';
  GS_GROUPE_INCLURE         = 'Inclure' ;
  GS_GROUPE_EXCLURE         = 'Exclure' ;
  GS_GROUPE_TOUT_INCLURE    = 'Tout inclure' ;
  GS_STRING_MUST_BE_HEXA    = 'La chaine doit représenter des hexadécimaux' ;
  GS_GROUPE_TOUT_EXCLURE    = 'Tout exclure' ;
  GS_GROUPE_RETOUR_ORIGINE  = 'Restaurer les données initiales' ;
  GS_GROUPE_MAUVAIS_BOUTONS = 'Les boutons de transfert doivent s''inverser dans les deux listes. ' + CST_ENDOFLINE
                        	+ 'Les boutons de transfert sont identifiés par rapport à leur liste,' + CST_ENDOFLINE
                        	+ ' à l''inverse des numéros d''images identifiés par rapport à la table. ' ;
      // Doit-on enregistrer ou abandonner
  GS_GROUPE_ABANDON = 'Veuillez enregistrer ou abandonner avant de continuer.' ;
      // Vidage du panier : oui ou non
  GS_GROUPE_VIDER   = 'Le panier utilisé pour les réaffectations n''est pas vide.' + CST_ENDOFLINE
                         + 'Voulez-vous abandonner ces réaffectations ?' ;
  GS_PAS_GROUPES    = 'DatasourceOwnerTable ou DatasourceOwnerKey non trouvés.' ;
  GS_GROUP_INCLUDE_LIST = 'Liste d''inclusion';
  GS_GROUP_EXCLUDE_LIST = 'Liste d''exclusion';

  GS_IMAGE_MAUVAISE_TAILLE = 'La taille de l''image doit être au moins de 32 sur 32.' ;
  GS_IMAGE_DEFORMATION = 'L''image sera déformée, continuer ?' ;
  GS_IMAGE_MAUVAISE_IMAGE = 'Mauvais format d''image.' ;
// Messages box
  gs_OK = 'OK';
  gs_Yes = 'Oui';
  gs_YesToAll = 'Oui à tout';
  gs_NoToAll = 'Non à tout';
  gs_No  = 'Non';
  gs_Cancel = 'Annuler';
  gs_Close = 'Fermer';
  gs_Ignore = 'Ignorer';
  gs_Retry = 'Réessayer';
  gs_Warning = 'Attention';
  gs_Information = 'Informations';
  gs_Confirmation = 'Confirmation';
  gs_Error = 'Erreur';
  gs_Please_Wait = 'Veuillez patienter…';
  gs_Press_ctrl_c_to_copy_text = 'Appuyez sur Ctrl+C pour copier ce texte.';
  gs_Downloading_in_progress = 'Téléchargement de @ARG en cours…';
  gs_Download_update = 'Télécharger mise à jour';
  gs_Download_finished = 'Téléchargement terminé.';
  gs_Send_Mail_Caption = 'Envoyer des mails' ;
  gs_Images_Format_out = 'Faut-il ajouter l''extension @ARG aux extensions d''images prohibées ?';
  GS_mot_passe_invalide = 'Mot de passe invalide.' + CST_ENDOFLINE
	 + 'Veuillez resaisir votre mot de passe.' ;
  GS_EXE_DO_NOT_EXISTS_EXITING   = 'L''exécutable suivant n''a pas été trouvé. L''application va s''arrếter.'+#10+'@ARG' ;
  GS_IMAGING_FILTER = 'Graphic (*.*)|*.bmp;*.BMP;*.xpm;*.XPM;'
          +'*.pbm;*.PBM;*.pgm;*.PGM;*.ppm;*.PPM;*.ico;*.ICO;*.icns;*.ICNS;*.cur;*.CUR;*.jpeg;*.JPEG;*.jpg;*.JPG;*.jpe;*.jfif;*.JFIF;*.tif;*.TIF;*.tiff;*.TIFF;*.gif;*.GIF;*.gif;*.GIF;'
          +'*.dagsky;*.dat;*.dagtexture;*.img;*.cif;*.rci;*.bsi;*.xpm;*.XPM;*.pcx;*.psd;*.pdd;*.jp2;*.j2k;'
          +'*.j2c;*.jpx;*.jpc;*.pfm;*.pam;*.ppm;*.PPM;*.pgm;*.PGM;*.pbm;*.PBM;*.tga;*.TGA;*.dds;*.gif;*.GIF;*.jng;*.JNG;*.mng;*.png;*.PNG;*.jpg;*.JPG;'
          +'*.jpeg;*.JPEG;*.jfif;*.JFIF;*.jpe;*.jif;*.bmp;*.BMP;*.dib;*.DIB;*.tga;*.TGA;*.dds;*.jng;*.JNG;*.mng;*.gif;*.GIF;*.png;*.PNG;*.jpg;*.JPG;*.jpeg;*.JPEG;'
          +'*.jfif;*.JFIF;*.jpe;*.jif;*.bmp;*.BMP;*.dib;*.DIB|Bitmaps (*.bmp;*.BMP)|*.bmp;*.BMP|Pixmap (*.xpm;*.XPM)|*.xpm;*.XPM|Portable PixMap'
          +' (*.pbm;*.PBM;*.pgm;*.PGM;*.ppm;*.PPM)|*.pbm;*.PBM;*.pgm;*.PGM;*.ppm;*.PPM|Icon (*.ico;*.ICO)|*.ico;*.ICO|Mac OS X Icon (*.icns;*.ICNS)|*.icns;*.ICNS|Cursor'
          +' (*.cur;*.CUR)|*.cur;*.CUR|Joint Picture Expert Group (*.jpeg;*.JPEG;*.jpg;*.JPG;*.jpe;*.jfif;*.JFIF)|*.jpeg;*.JPEG;*.jpg;*.JPG;*.jpe;*.jfif;*.JFIF|'
          +'Tagged Image File Format (*.tif;*.TIF;*.tiff;*.TIFF)|*.tif;*.TIF;*.tiff;*.TIFF|Graphics Interchange Format (*.gif;*.GIF)|*.gif;*.GIF|'
          +'Animated GIF (*.gif;*.GIF)|*.gif;*.GIF|Imaging Graphic AllInOne (*.dagsky)|*.dagsky|Imaging Graphic AllInOne'
          +' (*.dat)|*.dat|Imaging Graphic AllInOne (*.dagtexture)|*.dagtexture|Imaging Graphic AllInOne (*.img)'
          +'|*.img|Imaging Graphic AllInOne (*.cif)|*.cif|Imaging Graphic AllInOne (*.rci)|*.rci|Imaging Graphic '
          +'AllInOne (*.bsi)|*.bsi|Imaging Graphic AllInOne (*.xpm;*.XPM)|*.xpm;*.XPM|Imaging Graphic AllInOne (*.pcx)|*.pcx|'
          +'Imaging Graphic AllInOne (*.psd)|*.psd|Imaging Graphic AllInOne (*.pdd)|*.pdd|Imaging Graphic AllInOne'
          +' (*.jp2)|*.jp2|Imaging Graphic AllInOne (*.j2k)|*.j2k|Imaging Graphic AllInOne (*.j2c)|*.j2c|'
          +'Imaging Graphic AllInOne (*.jpx)|*.jpx|Imaging Graphic AllInOne (*.jpc)|*.jpc|Imaging Graphic AllInOne '
          +'(*.pfm)|*.pfm|Imaging Graphic AllInOne (*.pam)|*.pam|Imaging Graphic AllInOne (*.ppm;*.PPM)|*.ppm;*.PPM|'
          +'Imaging Graphic AllInOne (*.pgm;*.PGM)|*.pgm;*.PGM|Imaging Graphic AllInOne (*.pbm;*.PBM)|*.pbm;*.PBM|Imaging Graphic'
          +' AllInOne (*.tga;*.TGA)|*.tga;*.TGA|Imaging Graphic AllInOne (*.dds)|*.dds|Imaging Graphic AllInOne (*.gif;*.GIF)|'
          +'*.gif;*.GIF|Imaging Graphic AllInOne (*.jng;*.JNG)|*.jng;*.JNG|Imaging Graphic AllInOne (*.mng)|*.mng|'
          +'Imaging Graphic AllInOne (*.png;*.PNG)|*.png;*.PNG|Imaging Graphic AllInOne (*.jpg;*.JPG)|*.jpg;*.JPG|'
          +'Imaging Graphic AllInOne (*.jpeg;*.JPEG)|*.jpeg;*.JPEG|Imaging Graphic AllInOne (*.jfif;*.JFIF)|*.jfif;*.JFIF|'
          +'Imaging Graphic AllInOne (*.jpe)|*.jpe|Imaging Graphic AllInOne (*.jif)|*.jif|'
          +'Imaging Graphic AllInOne (*.bmp;*.BMP)|*.bmp;*.BMP|Imaging Graphic AllInOne (*.dib;*.DIB)|*.dib;*.DIB|'
          +'Truevision Targa Image (*.tga;*.TGA)|*.tga;*.TGA|DirectDraw Surface (*.dds)|*.dds|JPEG Network Graphics (*.jng;*.JNG)|'
          +'*.jng;*.JNG|Multiple Network Graphics (*.mng)|*.mng|Graphics Interchange Format (*.gif;*.GIF)|*.gif;*.GIF|'
          +'Portable Network Graphics (*.png;*.PNG)|*.png;*.PNG|Joint Photographic Experts Group Image (*.jpg;*.JPG)|*.jpg;*.JPG|'
          +'Joint Photographic Experts Group Image (*.jpeg;*.JPEG)|*.jpeg;*.JPEG|Joint Photographic Experts Group Image '
          +'(*.jfif;*.JFIF)|*.jfif;*.JFIF|Joint Photographic Experts Group Image (*.jpe)|*.jpe|Joint Photographic Experts Group Image (*.jif)|'
          +'*.jif|Windows Bitmap Image (*.bmp;*.BMP)|*.bmp;*.BMP|Windows Bitmap Image (*.dib;*.DIB)|*.dib;*.DIB|Tous les fichiers (*)|*|' ;

  // dbcomponents
  gs_Caption_Save_in = 'Sauvegarde dans @ARG.';
  gs_Caption_Restore_database = 'Restauration base @ARG.';
  gs_Error_Restore_Directory_does_not_exists =
          'Opération impossible, le répertoire de sauvegarde'+CST_ENDOFLINE
        +'@ARG'+CST_ENDOFLINE+'n''existe pas et ne peut être créé.';
  gs_Optimising_database_is_a_success = 'L''optimisation de la base s''est bien passée.';

 // Composants
  GS_SUPPRIMER_QUESTION = 'Confirmez-vous l''effacement de l''enregistrement ?' ;
  GS_CHARGEMENT_IMPOSSIBLE_FIELD_IMAGE  = 'Il est impossible de charger l''enregistrement de l''image.' ;
  GS_CHARGEMENT_IMPOSSIBLE_STREAM_IMAGE = 'Il est impossible de charger le flux image.' ;
  GS_CHARGEMENT_IMPOSSIBLE_STREAM_FIELD = 'Il est impossible de charger le flux image dans le champ.' ;
  GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE   = 'Il est impossible de charger le fichier image.' ;
  GS_ERREUR_OUVERTURE = 'Erreur à l''ouverture @ARG.' ;
  GS_FORM_ABANDON_OUVERTURE = 'Abandon de l''ouverture de la fiche @ARG...' ;
  GS_FirstRecord = 'Premier enregistrement';
  Gs_GROUPVIEW_Basket = 'Retour origine';
  Gs_GROUPVIEW_Record = 'Enregistrer';
  Gs_GROUPVIEW_Abort  = 'Abandonner';

  GS_PriorRecord = 'Enregistrement précédent';
  GS_NextRecord = 'Enregistrement suivant';
  GS_LastRecord = 'Dernier enregistrement';
  GS_InsertRecord = 'Insérer enregistrement';
  GS_DeleteRecord = 'Supprimer l''enregistrement';
  GS_DeleteFileNamed = 'Supprimer ce fichier :'+CST_ENDOFLINE+'@ARG ?';
  GS_EditRecord = 'Modifier l''enregistrement';
  GS_PostEdit = 'Valider modifications';
  GS_CancelEdit = 'Annuler les modifications';
  GS_ConfirmCaption = 'Confirmation';
  GS_RefreshRecord = 'Rafraîchir les données';
  GS_SearchRecord = 'Rechercher' ;
  GS_MoveNextRecord = 'Déplacer l''enregistrement au suivant' ;
  GS_MovePreviousRecord = 'Déplacer l''enregistrement au précédent' ;
  GS_SetBookmarkRecord = 'Marquer L''enregistrement' ;
  GS_GotoBookmarkRecord = 'Aller à l''enregistrement Marqué' ;

  // SGBD
 
 {$IFDEF FPC}
  SCloseButton = '&Fermer' ;
 {$ENDIF}

  // Erreurs
  gs_Error_Forbidden_Access = 'Accès interdit';
  gs_Error_Bad_URL = 'Mauvaise URL : @ARG';
  gs_Error_Bad_request = 'Mauvaise requête';
  gs_Error_Bad_Gateway = 'Mauvaise passerelle';
  gs_Error_Bad_Web_Connection='Problème de connexion internet.';
  gs_Error_Bad_Connection='Problème de connexion à @ARG.';
  gs_Error_Cannot_load_not_downloaded_file = 'Impossible de lire le fichier. Il n''a pas été téléchargé.';
  gs_Error_Cannot_Write_on = 'Écriture impossible de @ARG.';
  gs_Error_File_is_not_on_the_web_site = 'Le fichier que vous essayez de télécharger est absent du site.';
  gs_Error_timeout_problem = 'Problème de timeOut';
  GS_ERREUR_NOMBRE_GRAND = 'Problème à la validation du nombre :' + CST_ENDOFLINE
                   + 'Un nombre saisi est trop grand.' + CST_ENDOFLINE
                   + 'Modifier la saisie ou annuler.' ;
  GS_METTRE_A_JOUR_FICHE = 'L''enregistrement a été effacé ou modifié par un autre utilisateur.' + #13 + #10
                        			+ 'La fiche va être mise à jour.' ;
  GS_VALEUR_UTILISEE   = 'La valeur @ARG est déjà utilisée.' + #13 + #10
                        		+ 'Saisir une valeur différente, annuler ou réeffectuer la validation si une valeur n''est pas modifiable.' ;
  GS_VALEURS_UTILISEES = 'Les valeurs @ARG sont déjà utilisées.' + #13 + #10
                        		+ 'Saisir des valeurs différentes, annuler ou réeffectuer la validation si une valeur n''est pas modifiable.' ;
  GS_ERREUR_RESEAU = 'Erreur réseau.' + CST_ENDOFLINE
                        + 'Vérifier la connexion réseau.' ;
  GS_ERREUR_MODIFICATION_MAJ = 'Impossible de supprimer cet enregistrement.' + CST_ENDOFLINE
               + 'Il est utilisé dans une autre fonction.';
  GS_ERREUR_DONNEES = 'Un problème est survenu dans l''accès aux données.' + CST_ENDOFLINE;
  GS_ERREUR_CONNEXION = 'Un problème est survenu pour la connexion aux données.' + CST_ENDOFLINE
                        	 + 'Réessayez d''ouvrir la fiche.' ;
                        //GS_CHANGEMENTS_SAUVER = 'Des changements ont été effectués.' + CST_ENDOFLINE +' Le trie nécessite alors une sauvegarde.'  + CST_ENDOFLINE + 'Voulez-vous enregistrer les changements effectués ?' ;

 // Messages pour les images
  GS_IMAGE_MULTIPLE_5 = 'L''image doit avoir une largeur multiple de ' ;
  GS_IMAGE_DOIT_ETRE = 'La taille de l''image doit être de ' ;
  GS_IMAGE_HAUTEUR = 'L''image doit avoir une hauteur de ' ;
  GS_IMAGE_TROP_PETITE = 'L''image est trop petite.' ;
  GS_IMAGE_TROP_GRANDE = 'L''image est trop grande.' ;
  GS_FICHIER_NON_TROUVE = 'Fichier @ARG non trouvé.' ;

  // confirmation demandée
  GS_INI_FILE_CANT_WRITE = 'Chemin @ARG inaccessible.' + CST_ENDOFLINE + 'Démarrer l''application en tant qu''administrateur' ;
  gs_Confirm_File_is_unavailable_Do_i_erase_it_to_update_it = 'Le fichier est innaccessible.'+CST_ENDOFLINE+'L''effacer pour le mettre à jour ?';

 // Messages pour le navigateur
  GS_INSERER_ENREGISTREMENT = 'Insérer un enregistrement' ;
  GS_VALIDER_MODIFICATIONS  = 'Valider les modifications' ;

  GS_NAVIGATEUR_VERS_LE_BAS  = 'Déplacer la ligne vers le bas' ;
  GS_NAVIGATEUR_VERS_LE_HAUT = 'Déplacer la ligne vers le haut' ;

  GS_PB_CONNEXION = 'La connexion a échouée.' + #13 + #10
                      + 'Veuillez contacter votre administrateur.';
  GS_DECONNECTER_ANNULE = 'Annulation de la déconnexion';
  GS_Files_seems_to_be_the_same_reuse = 'Il y a un fichier de même taille et nom déjà sauvegardé. Le réutilise-t-on ?';

  GS_MODE_ASYNCHRONE = 'Mode Asynchrone' ;
  GS_ACCES_DIRECT_SERVEUR = 'Accès directs Serveur' ;
  GS_MODE_CONNEXION_ASYNCHRONE = 'Connection Asynchrone' ;
  GS_MODE_ASYNCHRONE_NB_ENREGISTREMENTS = 'Mode Asynchrone Enregistrements' ;
  GS_MODE_ASYNCHRONE_TIMEOUT = 'Mode Asynchrone TimeOut' ;
  GS_CONNECTION_TIMEOUT = 'Connection TimeOut' ;
  GS_Set_KEYSET         = 'Set Keyset';

var
  gb_MainFormIniOneUserOnServer : Boolean = False ;
{$IFDEF EADO}
  GB_ASYNCHRONE_PAR_DEFAUT : Boolean = False ;
{$ENDIF}
implementation

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_unite_messages );
{$ENDIF}
end.
