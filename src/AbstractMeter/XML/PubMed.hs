module PubMed where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN


{-Type decls-}

data MedlineCitationSet = MedlineCitationSet [MedlineCitation]
                                             (Maybe DeleteCitation)
                        deriving (Eq,Show)
data MedlineCitation = MedlineCitation MedlineCitation_Attrs PMID
                                       DateCreated (Maybe DateCompleted) (Maybe DateRevised) Article
                                       MedlineJournalInfo (Maybe ChemicalList) [CitationSubset]
                                       (Maybe CommentsCorrectionsList) (Maybe GeneSymbolList)
                                       (Maybe MeshHeadingList) (Maybe NumberOfReferences)
                                       (Maybe PersonalNameSubjectList) [OtherID] [OtherAbstract]
                                       [KeywordList] [SpaceFlightMission] (Maybe InvestigatorList)
                                       [GeneralNote]
                     deriving (Eq,Show)
data MedlineCitation_Attrs = MedlineCitation_Attrs
    { medlineCitationOwner :: (Defaultable MedlineCitation_Owner)
    , medlineCitationStatus :: MedlineCitation_Status
    } deriving (Eq,Show)
data MedlineCitation_Owner = MedlineCitation_Owner_NLM  | 
                             MedlineCitation_Owner_NASA  |  MedlineCitation_Owner_PIP  | 
                             MedlineCitation_Owner_KIE  |  MedlineCitation_Owner_HSR  | 
                             MedlineCitation_Owner_HMD  |  MedlineCitation_Owner_NOTNLM
                           deriving (Eq,Show)
data MedlineCitation_Status = MedlineCitation_Status_Completed  | 
                              MedlineCitation_Status_In_Process  | 
                              MedlineCitation_Status_PubMed_not_MEDLINE  | 
                              MedlineCitation_Status_In_Data_Review  | 
                              MedlineCitation_Status_Publisher  |  MedlineCitation_Status_MEDLINE
                               |  MedlineCitation_Status_OLDMEDLINE
                            deriving (Eq,Show)
data Abstract = Abstract AbstractText (Maybe CopyrightInformation)
              deriving (Eq,Show)
newtype AbstractText = AbstractText String 		deriving (Eq,Show)
newtype AccessionNumber = AccessionNumber String 		deriving (Eq,Show)
newtype AccessionNumberList = AccessionNumberList (List1 AccessionNumber) 		deriving (Eq,Show)
newtype Acronym = Acronym String 		deriving (Eq,Show)
newtype Affiliation = Affiliation String 		deriving (Eq,Show)
newtype Agency = Agency String 		deriving (Eq,Show)
data Article = Article Article_Attrs Journal ArticleTitle
                       (OneOf2 (Pagination,[ELocationID]) (List1 ELocationID))
                       (Maybe Abstract) (Maybe Affiliation) (Maybe AuthorList)
                       (List1 Language) (Maybe DataBankList) (Maybe GrantList)
                       PublicationTypeList (Maybe VernacularTitle) [ArticleDate]
             deriving (Eq,Show)
data Article_Attrs = Article_Attrs
    { articlePubModel :: Article_PubModel
    } deriving (Eq,Show)
data Article_PubModel = Article_PubModel_Print  | 
                        Article_PubModel_Print_Electronic  |  Article_PubModel_Electronic
                         |  Article_PubModel_Electronic_Print
                      deriving (Eq,Show)
data ArticleDate = ArticleDate ArticleDate_Attrs Year Month Day
                 deriving (Eq,Show)
data ArticleDate_Attrs = ArticleDate_Attrs
    { articleDateDateType :: (Defaultable String)
    } deriving (Eq,Show)
newtype ArticleTitle = ArticleTitle String 		deriving (Eq,Show)
data Author = AuthorLastName_ForeName_Initials_Suffix_NameID Author_Attrs
                                                             (LastName,(Maybe ForeName),(Maybe Initials),(Maybe Suffix),[NameID])
            | AuthorCollectiveName_NameID Author_Attrs
                                          (CollectiveName,[NameID])
            deriving (Eq,Show)
data Author_Attrs = Author_Attrs
    { authorValidYN :: (Defaultable Author_ValidYN)
    } deriving (Eq,Show)
data Author_ValidYN = Author_ValidYN_Y  |  Author_ValidYN_N
                    deriving (Eq,Show)
data AuthorList = AuthorList AuthorList_Attrs (List1 Author)
                deriving (Eq,Show)
data AuthorList_Attrs = AuthorList_Attrs
    { authorListCompleteYN :: (Defaultable AuthorList_CompleteYN)
    } deriving (Eq,Show)
data AuthorList_CompleteYN = AuthorList_CompleteYN_Y  | 
                             AuthorList_CompleteYN_N
                           deriving (Eq,Show)
data Chemical = Chemical RegistryNumber NameOfSubstance
              deriving (Eq,Show)
newtype ChemicalList = ChemicalList (List1 Chemical) 		deriving (Eq,Show)
newtype CitationSubset = CitationSubset String 		deriving (Eq,Show)
newtype CollectiveName = CollectiveName String 		deriving (Eq,Show)
data CommentsCorrections = CommentsCorrections CommentsCorrections_Attrs
                                               RefSource (Maybe PMID) (Maybe Note)
                         deriving (Eq,Show)
data CommentsCorrections_Attrs = CommentsCorrections_Attrs
    { commentsCorrectionsRefType :: CommentsCorrections_RefType
    } deriving (Eq,Show)
data CommentsCorrections_RefType = CommentsCorrections_RefType_CommentOn
                                    |  CommentsCorrections_RefType_CommentIn  | 
                                   CommentsCorrections_RefType_ErratumIn  | 
                                   CommentsCorrections_RefType_ErratumFor  | 
                                   CommentsCorrections_RefType_PartialRetractionIn  | 
                                   CommentsCorrections_RefType_PartialRetractionOf  | 
                                   CommentsCorrections_RefType_RepublishedFrom  | 
                                   CommentsCorrections_RefType_RepublishedIn  | 
                                   CommentsCorrections_RefType_RetractionOf  | 
                                   CommentsCorrections_RefType_RetractionIn  | 
                                   CommentsCorrections_RefType_UpdateIn  | 
                                   CommentsCorrections_RefType_UpdateOf  | 
                                   CommentsCorrections_RefType_SummaryForPatientsIn  | 
                                   CommentsCorrections_RefType_OriginalReportIn  | 
                                   CommentsCorrections_RefType_ReprintOf  | 
                                   CommentsCorrections_RefType_ReprintIn  | 
                                   CommentsCorrections_RefType_Cites
                                 deriving (Eq,Show)
newtype CommentsCorrectionsList = CommentsCorrectionsList (List1 CommentsCorrections) 		deriving (Eq,Show)
newtype CopyrightInformation = CopyrightInformation String 		deriving (Eq,Show)
newtype Country = Country String 		deriving (Eq,Show)
data DataBank = DataBank DataBankName (Maybe AccessionNumberList)
              deriving (Eq,Show)
data DataBankList = DataBankList DataBankList_Attrs
                                 (List1 DataBank)
                  deriving (Eq,Show)
data DataBankList_Attrs = DataBankList_Attrs
    { dataBankListCompleteYN :: (Defaultable DataBankList_CompleteYN)
    } deriving (Eq,Show)
data DataBankList_CompleteYN = DataBankList_CompleteYN_Y  | 
                               DataBankList_CompleteYN_N
                             deriving (Eq,Show)
newtype DataBankName = DataBankName String 		deriving (Eq,Show)
data DateCompleted = DateCompleted Year Month Day
                   deriving (Eq,Show)
data DateCreated = DateCreated Year Month Day
                 deriving (Eq,Show)
data DateRevised = DateRevised Year Month Day
                 deriving (Eq,Show)
newtype Day = Day String 		deriving (Eq,Show)
data DescriptorName = DescriptorName DescriptorName_Attrs String
                    deriving (Eq,Show)
data DescriptorName_Attrs = DescriptorName_Attrs
    { descriptorNameMajorTopicYN :: (Defaultable DescriptorName_MajorTopicYN)
    } deriving (Eq,Show)
data DescriptorName_MajorTopicYN = DescriptorName_MajorTopicYN_Y
                                    |  DescriptorName_MajorTopicYN_N
                                 deriving (Eq,Show)
data ELocationID = ELocationID ELocationID_Attrs String
                 deriving (Eq,Show)
data ELocationID_Attrs = ELocationID_Attrs
    { eLocationIDEIdType :: ELocationID_EIdType
    , eLocationIDValidYN :: (Defaultable ELocationID_ValidYN)
    } deriving (Eq,Show)
data ELocationID_EIdType = ELocationID_EIdType_doi  | 
                           ELocationID_EIdType_pii
                         deriving (Eq,Show)
data ELocationID_ValidYN = ELocationID_ValidYN_Y  | 
                           ELocationID_ValidYN_N
                         deriving (Eq,Show)
newtype EndPage = EndPage String 		deriving (Eq,Show)
newtype ForeName = ForeName String 		deriving (Eq,Show)
newtype GeneSymbol = GeneSymbol String 		deriving (Eq,Show)
newtype GeneSymbolList = GeneSymbolList (List1 GeneSymbol) 		deriving (Eq,Show)
data GeneralNote = GeneralNote GeneralNote_Attrs String
                 deriving (Eq,Show)
data GeneralNote_Attrs = GeneralNote_Attrs
    { generalNoteOwner :: (Defaultable GeneralNote_Owner)
    } deriving (Eq,Show)
data GeneralNote_Owner = GeneralNote_Owner_NLM  | 
                         GeneralNote_Owner_NASA  |  GeneralNote_Owner_PIP  | 
                         GeneralNote_Owner_KIE  |  GeneralNote_Owner_HSR  | 
                         GeneralNote_Owner_HMD
                       deriving (Eq,Show)
data Grant = Grant (Maybe GrantID) (Maybe Acronym) Agency Country
           deriving (Eq,Show)
newtype GrantID = GrantID String 		deriving (Eq,Show)
data GrantList = GrantList GrantList_Attrs (List1 Grant)
               deriving (Eq,Show)
data GrantList_Attrs = GrantList_Attrs
    { grantListCompleteYN :: (Defaultable GrantList_CompleteYN)
    } deriving (Eq,Show)
data GrantList_CompleteYN = GrantList_CompleteYN_Y  | 
                            GrantList_CompleteYN_N
                          deriving (Eq,Show)
newtype ISOAbbreviation = ISOAbbreviation String 		deriving (Eq,Show)
data ISSN = ISSN ISSN_Attrs String
          deriving (Eq,Show)
data ISSN_Attrs = ISSN_Attrs
    { iSSNIssnType :: ISSN_IssnType
    } deriving (Eq,Show)
data ISSN_IssnType = ISSN_IssnType_Electronic  | 
                     ISSN_IssnType_Print
                   deriving (Eq,Show)
newtype ISSNLinking = ISSNLinking String 		deriving (Eq,Show)
newtype Initials = Initials String 		deriving (Eq,Show)
data Investigator = Investigator Investigator_Attrs LastName
                                 (Maybe ForeName) (Maybe Initials) (Maybe Suffix) [NameID]
                                 (Maybe Affiliation)
                  deriving (Eq,Show)
data Investigator_Attrs = Investigator_Attrs
    { investigatorValidYN :: (Defaultable Investigator_ValidYN)
    } deriving (Eq,Show)
data Investigator_ValidYN = Investigator_ValidYN_Y  | 
                            Investigator_ValidYN_N
                          deriving (Eq,Show)
newtype InvestigatorList = InvestigatorList (List1 Investigator) 		deriving (Eq,Show)
newtype Issue = Issue String 		deriving (Eq,Show)
data Journal = Journal (Maybe ISSN) JournalIssue (Maybe Title)
                       (Maybe ISOAbbreviation)
             deriving (Eq,Show)
data JournalIssue = JournalIssue JournalIssue_Attrs (Maybe Volume)
                                 (Maybe Issue) PubDate
                  deriving (Eq,Show)
data JournalIssue_Attrs = JournalIssue_Attrs
    { journalIssueCitedMedium :: JournalIssue_CitedMedium
    } deriving (Eq,Show)
data JournalIssue_CitedMedium = JournalIssue_CitedMedium_Internet
                                 |  JournalIssue_CitedMedium_Print
                              deriving (Eq,Show)
data Keyword = Keyword Keyword_Attrs String
             deriving (Eq,Show)
data Keyword_Attrs = Keyword_Attrs
    { keywordMajorTopicYN :: (Defaultable Keyword_MajorTopicYN)
    } deriving (Eq,Show)
data Keyword_MajorTopicYN = Keyword_MajorTopicYN_Y  | 
                            Keyword_MajorTopicYN_N
                          deriving (Eq,Show)
data KeywordList = KeywordList KeywordList_Attrs (List1 Keyword)
                 deriving (Eq,Show)
data KeywordList_Attrs = KeywordList_Attrs
    { keywordListOwner :: (Defaultable KeywordList_Owner)
    } deriving (Eq,Show)
data KeywordList_Owner = KeywordList_Owner_NLM  | 
                         KeywordList_Owner_NASA  |  KeywordList_Owner_PIP  | 
                         KeywordList_Owner_KIE  |  KeywordList_Owner_NOTNLM
                       deriving (Eq,Show)
newtype Language = Language String 		deriving (Eq,Show)
newtype LastName = LastName String 		deriving (Eq,Show)
newtype MedlineDate = MedlineDate String 		deriving (Eq,Show)
data MedlineJournalInfo = MedlineJournalInfo (Maybe Country)
                                             MedlineTA (Maybe NlmUniqueID) (Maybe ISSNLinking)
                        deriving (Eq,Show)
newtype MedlinePgn = MedlinePgn String 		deriving (Eq,Show)
newtype MedlineTA = MedlineTA String 		deriving (Eq,Show)
data MeshHeading = MeshHeading DescriptorName [QualifierName]
                 deriving (Eq,Show)
newtype MeshHeadingList = MeshHeadingList (List1 MeshHeading) 		deriving (Eq,Show)
newtype Month = Month String 		deriving (Eq,Show)
data NameID = NameID NameID_Attrs String
            deriving (Eq,Show)
data NameID_Attrs = NameID_Attrs
    { nameIDSource :: NameID_Source
    } deriving (Eq,Show)
data NameID_Source = NameID_Source_NCBI  |  NameID_Source_Publisher
                      |  NameID_Source_NISO  |  NameID_Source_ISO
                   deriving (Eq,Show)
newtype NameOfSubstance = NameOfSubstance String 		deriving (Eq,Show)
newtype NlmUniqueID = NlmUniqueID String 		deriving (Eq,Show)
newtype Note = Note String 		deriving (Eq,Show)
newtype NumberOfReferences = NumberOfReferences String 		deriving (Eq,Show)
data OtherAbstract = OtherAbstract OtherAbstract_Attrs AbstractText
                                   (Maybe CopyrightInformation)
                   deriving (Eq,Show)
data OtherAbstract_Attrs = OtherAbstract_Attrs
    { otherAbstractType :: OtherAbstract_Type
    } deriving (Eq,Show)
data OtherAbstract_Type = OtherAbstract_Type_AAMC  | 
                          OtherAbstract_Type_AIDS  |  OtherAbstract_Type_KIE  | 
                          OtherAbstract_Type_PIP  |  OtherAbstract_Type_NASA  | 
                          OtherAbstract_Type_Publisher
                        deriving (Eq,Show)
data OtherID = OtherID OtherID_Attrs String
             deriving (Eq,Show)
data OtherID_Attrs = OtherID_Attrs
    { otherIDSource :: OtherID_Source
    } deriving (Eq,Show)
data OtherID_Source = OtherID_Source_NASA  |  OtherID_Source_KIE
                       |  OtherID_Source_PIP  |  OtherID_Source_POP  | 
                      OtherID_Source_ARPL  |  OtherID_Source_CPC  |  OtherID_Source_IND
                       |  OtherID_Source_CPFH  |  OtherID_Source_CLML  | 
                      OtherID_Source_NRCBL  |  OtherID_Source_NLM
                    deriving (Eq,Show)
newtype PMID = PMID String 		deriving (Eq,Show)
data Pagination = PaginationStartPage_EndPage_MedlinePgn (StartPage,(Maybe EndPage),(Maybe MedlinePgn))
                | PaginationMedlinePgn MedlinePgn
                deriving (Eq,Show)
data PersonalNameSubject = PersonalNameSubject LastName
                                               (Maybe ForeName) (Maybe Initials) (Maybe Suffix)
                         deriving (Eq,Show)
newtype PersonalNameSubjectList = PersonalNameSubjectList (List1 PersonalNameSubject) 		deriving (Eq,Show)
data PubDate = PubDateYear_Month_Day_Season (Year,(Maybe (OneOf2 (Month,(Maybe Day)) Season)))
             | PubDateMedlineDate MedlineDate
             deriving (Eq,Show)
newtype PublicationType = PublicationType String 		deriving (Eq,Show)
newtype PublicationTypeList = PublicationTypeList (List1 PublicationType) 		deriving (Eq,Show)
data QualifierName = QualifierName QualifierName_Attrs String
                   deriving (Eq,Show)
data QualifierName_Attrs = QualifierName_Attrs
    { qualifierNameMajorTopicYN :: (Defaultable QualifierName_MajorTopicYN)
    } deriving (Eq,Show)
data QualifierName_MajorTopicYN = QualifierName_MajorTopicYN_Y  | 
                                  QualifierName_MajorTopicYN_N
                                deriving (Eq,Show)
newtype RefSource = RefSource String 		deriving (Eq,Show)
newtype RegistryNumber = RegistryNumber String 		deriving (Eq,Show)
newtype Season = Season String 		deriving (Eq,Show)
newtype SpaceFlightMission = SpaceFlightMission String 		deriving (Eq,Show)
newtype StartPage = StartPage String 		deriving (Eq,Show)
newtype Suffix = Suffix String 		deriving (Eq,Show)
newtype Title = Title String 		deriving (Eq,Show)
newtype VernacularTitle = VernacularTitle String 		deriving (Eq,Show)
newtype Volume = Volume String 		deriving (Eq,Show)
newtype Year = Year String 		deriving (Eq,Show)
newtype DeleteCitation = DeleteCitation (List1 PMID) 		deriving (Eq,Show)
newtype PubmedArticleSet = PubmedArticleSet (List1 PubmedArticle) 		deriving (Eq,Show)
data PubmedArticle = PubmedArticle MedlineCitation
                                   (Maybe PubmedData)
                   deriving (Eq,Show)
data PubmedData = PubmedData (Maybe History) PublicationStatus
                             ArticleIdList (Maybe ObjectList)
                deriving (Eq,Show)
data PubMedPubDate = PubMedPubDate PubMedPubDate_Attrs Year Month
                                   Day (Maybe (Hour,(Maybe (Minute,(Maybe Second)))))
                   deriving (Eq,Show)
data PubMedPubDate_Attrs = PubMedPubDate_Attrs
    { pubMedPubDatePubStatus :: PubMedPubDate_PubStatus
    } deriving (Eq,Show)
data PubMedPubDate_PubStatus = PubMedPubDate_PubStatus_received  | 
                               PubMedPubDate_PubStatus_accepted  | 
                               PubMedPubDate_PubStatus_epublish  | 
                               PubMedPubDate_PubStatus_ppublish  | 
                               PubMedPubDate_PubStatus_revised  | 
                               PubMedPubDate_PubStatus_aheadofprint  | 
                               PubMedPubDate_PubStatus_retracted  |  PubMedPubDate_PubStatus_pmc
                                |  PubMedPubDate_PubStatus_pmcr  |  PubMedPubDate_PubStatus_pubmed
                                |  PubMedPubDate_PubStatus_pubmedr  | 
                               PubMedPubDate_PubStatus_premedline  | 
                               PubMedPubDate_PubStatus_medline  | 
                               PubMedPubDate_PubStatus_medliner  |  PubMedPubDate_PubStatus_entrez
                                |  PubMedPubDate_PubStatus_pmc_release
                             deriving (Eq,Show)
newtype PublicationStatus = PublicationStatus String 		deriving (Eq,Show)
newtype ArticleIdList = ArticleIdList (List1 ArticleId) 		deriving (Eq,Show)
data ArticleId = ArticleId ArticleId_Attrs String
               deriving (Eq,Show)
data ArticleId_Attrs = ArticleId_Attrs
    { articleIdIdType :: (Defaultable ArticleId_IdType)
    } deriving (Eq,Show)
data ArticleId_IdType = ArticleId_IdType_doi  | 
                        ArticleId_IdType_pii  |  ArticleId_IdType_pmcpid  | 
                        ArticleId_IdType_pmpid  |  ArticleId_IdType_pmc  | 
                        ArticleId_IdType_mid  |  ArticleId_IdType_sici  | 
                        ArticleId_IdType_pubmed  |  ArticleId_IdType_medline  | 
                        ArticleId_IdType_pmcid
                      deriving (Eq,Show)
newtype History = History (List1 PubMedPubDate) 		deriving (Eq,Show)
data URL = URL URL_Attrs String
         deriving (Eq,Show)
data URL_Attrs = URL_Attrs
    { uRLLang :: (Maybe URL_lang)
    , uRLType :: (Maybe URL_Type)
    } deriving (Eq,Show)
data URL_lang = URL_lang_AF  |  URL_lang_AR  |  URL_lang_AZ  | 
                URL_lang_BG  |  URL_lang_CS  |  URL_lang_DA  |  URL_lang_DE  | 
                URL_lang_EN  |  URL_lang_EL  |  URL_lang_ES  |  URL_lang_FA  | 
                URL_lang_FI  |  URL_lang_FR  |  URL_lang_HE  |  URL_lang_HU  | 
                URL_lang_HY  |  URL_lang_IN  |  URL_lang_IS  |  URL_lang_IT  | 
                URL_lang_IW  |  URL_lang_JA  |  URL_lang_KA  |  URL_lang_KO  | 
                URL_lang_LT  |  URL_lang_MK  |  URL_lang_ML  |  URL_lang_NL  | 
                URL_lang_NO  |  URL_lang_PL  |  URL_lang_PT  |  URL_lang_PS  | 
                URL_lang_RO  |  URL_lang_RU  |  URL_lang_SL  |  URL_lang_SK  | 
                URL_lang_SQ  |  URL_lang_SR  |  URL_lang_SV  |  URL_lang_SW  | 
                URL_lang_TH  |  URL_lang_TR  |  URL_lang_UK  |  URL_lang_VI  | 
                URL_lang_ZH
              deriving (Eq,Show)
data URL_Type = URL_Type_FullText  |  URL_Type_Summary  | 
                URL_Type_fulltext  |  URL_Type_summary
              deriving (Eq,Show)
newtype ObjectList = ObjectList (List1 Object) 		deriving (Eq,Show)
data Object = Object Object_Attrs [Param]
            deriving (Eq,Show)
data Object_Attrs = Object_Attrs
    { objectType :: String
    } deriving (Eq,Show)
data Param = Param Param_Attrs String
           deriving (Eq,Show)
data Param_Attrs = Param_Attrs
    { paramName :: String
    } deriving (Eq,Show)
newtype Hour = Hour String 		deriving (Eq,Show)
newtype Minute = Minute String 		deriving (Eq,Show)
newtype Second = Second String 		deriving (Eq,Show)


{-Instance decls-}

instance HTypeable MedlineCitationSet where
    toHType x = Defined "MedlineCitationSet" [] []
instance XmlContent MedlineCitationSet where
    toContents (MedlineCitationSet a b) =
        [CElem (Elem (N "MedlineCitationSet") [] (concatMap toContents a ++
                                                  maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MedlineCitationSet"]
        ; interior e $ return (MedlineCitationSet)
                       `apply` many parseContents `apply` optional parseContents
        } `adjustErr` ("in <MedlineCitationSet>, "++)

instance HTypeable MedlineCitation where
    toHType x = Defined "MedlineCitation" [] []
instance XmlContent MedlineCitation where
    toContents (MedlineCitation as a b c d e f g h i j k l m n o p q r
                                   s) =
        [CElem (Elem (N "MedlineCitation") (toAttrs as) (toContents a ++
                                                         toContents b ++ maybe [] toContents c ++
                                                         maybe [] toContents d ++ toContents e ++
                                                         toContents f ++ maybe [] toContents g ++
                                                         concatMap toContents h ++
                                                         maybe [] toContents i ++
                                                         maybe [] toContents j ++
                                                         maybe [] toContents k ++
                                                         maybe [] toContents l ++
                                                         maybe [] toContents m ++
                                                         concatMap toContents n ++
                                                         concatMap toContents o ++
                                                         concatMap toContents p ++
                                                         concatMap toContents q ++
                                                         maybe [] toContents r ++
                                                         concatMap toContents s)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["MedlineCitation"]
        ; interior e $ return (MedlineCitation (fromAttrs as))
                       `apply` parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` parseContents `apply` parseContents
                       `apply` optional parseContents `apply` many parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` optional parseContents `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
                       `apply` many parseContents `apply` optional parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <MedlineCitation>, "++)
instance XmlAttributes MedlineCitation_Attrs where
    fromAttrs as =
        MedlineCitation_Attrs
          { medlineCitationOwner = defaultA fromAttrToTyp MedlineCitation_Owner_NLM "Owner" as
          , medlineCitationStatus = definiteA fromAttrToTyp "MedlineCitation" "Status" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "Owner" (medlineCitationOwner v)
        , toAttrFrTyp "Status" (medlineCitationStatus v)
        ]

instance XmlAttrType MedlineCitation_Owner where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "NLM" = Just MedlineCitation_Owner_NLM
            translate "NASA" = Just MedlineCitation_Owner_NASA
            translate "PIP" = Just MedlineCitation_Owner_PIP
            translate "KIE" = Just MedlineCitation_Owner_KIE
            translate "HSR" = Just MedlineCitation_Owner_HSR
            translate "HMD" = Just MedlineCitation_Owner_HMD
            translate "NOTNLM" = Just MedlineCitation_Owner_NOTNLM
            translate _ = Nothing
    toAttrFrTyp n MedlineCitation_Owner_NLM = Just (N n, str2attr "NLM")
    toAttrFrTyp n MedlineCitation_Owner_NASA = Just (N n, str2attr "NASA")
    toAttrFrTyp n MedlineCitation_Owner_PIP = Just (N n, str2attr "PIP")
    toAttrFrTyp n MedlineCitation_Owner_KIE = Just (N n, str2attr "KIE")
    toAttrFrTyp n MedlineCitation_Owner_HSR = Just (N n, str2attr "HSR")
    toAttrFrTyp n MedlineCitation_Owner_HMD = Just (N n, str2attr "HMD")
    toAttrFrTyp n MedlineCitation_Owner_NOTNLM = Just (N n, str2attr "NOTNLM")

instance XmlAttrType MedlineCitation_Status where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Completed" = Just MedlineCitation_Status_Completed
            translate "In-Process" = Just MedlineCitation_Status_In_Process
            translate "PubMed-not-MEDLINE" = Just MedlineCitation_Status_PubMed_not_MEDLINE
            translate "In-Data-Review" = Just MedlineCitation_Status_In_Data_Review
            translate "Publisher" = Just MedlineCitation_Status_Publisher
            translate "MEDLINE" = Just MedlineCitation_Status_MEDLINE
            translate "OLDMEDLINE" = Just MedlineCitation_Status_OLDMEDLINE
            translate _ = Nothing
    toAttrFrTyp n MedlineCitation_Status_Completed = Just (N n, str2attr "Completed")
    toAttrFrTyp n MedlineCitation_Status_In_Process = Just (N n, str2attr "In-Process")
    toAttrFrTyp n MedlineCitation_Status_PubMed_not_MEDLINE = Just (N n, str2attr "PubMed-not-MEDLINE")
    toAttrFrTyp n MedlineCitation_Status_In_Data_Review = Just (N n, str2attr "In-Data-Review")
    toAttrFrTyp n MedlineCitation_Status_Publisher = Just (N n, str2attr "Publisher")
    toAttrFrTyp n MedlineCitation_Status_MEDLINE = Just (N n, str2attr "MEDLINE")
    toAttrFrTyp n MedlineCitation_Status_OLDMEDLINE = Just (N n, str2attr "OLDMEDLINE")

instance HTypeable Abstract where
    toHType x = Defined "Abstract" [] []
instance XmlContent Abstract where
    toContents (Abstract a b) =
        [CElem (Elem (N "Abstract") [] (toContents a ++
                                        maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Abstract"]
        ; interior e $ return (Abstract) `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <Abstract>, "++)

instance HTypeable AbstractText where
    toHType x = Defined "AbstractText" [] []
instance XmlContent AbstractText where
    toContents (AbstractText a) =
        [CElem (Elem (N "AbstractText") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["AbstractText"]
        ; interior e $ return (AbstractText)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <AbstractText>, "++)

instance HTypeable AccessionNumber where
    toHType x = Defined "AccessionNumber" [] []
instance XmlContent AccessionNumber where
    toContents (AccessionNumber a) =
        [CElem (Elem (N "AccessionNumber") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["AccessionNumber"]
        ; interior e $ return (AccessionNumber)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <AccessionNumber>, "++)

instance HTypeable AccessionNumberList where
    toHType x = Defined "AccessionNumberList" [] []
instance XmlContent AccessionNumberList where
    toContents (AccessionNumberList a) =
        [CElem (Elem (N "AccessionNumberList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["AccessionNumberList"]
        ; interior e $ return (AccessionNumberList) `apply` parseContents
        } `adjustErr` ("in <AccessionNumberList>, "++)

instance HTypeable Acronym where
    toHType x = Defined "Acronym" [] []
instance XmlContent Acronym where
    toContents (Acronym a) =
        [CElem (Elem (N "Acronym") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Acronym"]
        ; interior e $ return (Acronym) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Acronym>, "++)

instance HTypeable Affiliation where
    toHType x = Defined "Affiliation" [] []
instance XmlContent Affiliation where
    toContents (Affiliation a) =
        [CElem (Elem (N "Affiliation") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Affiliation"]
        ; interior e $ return (Affiliation)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <Affiliation>, "++)

instance HTypeable Agency where
    toHType x = Defined "Agency" [] []
instance XmlContent Agency where
    toContents (Agency a) =
        [CElem (Elem (N "Agency") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Agency"]
        ; interior e $ return (Agency) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Agency>, "++)

instance HTypeable Article where
    toHType x = Defined "Article" [] []
instance XmlContent Article where
    toContents (Article as a b c d e f g h i j k l) =
        [CElem (Elem (N "Article") (toAttrs as) (toContents a ++
                                                 toContents b ++ toContents c ++
                                                 maybe [] toContents d ++ maybe [] toContents e ++
                                                 maybe [] toContents f ++ toContents g ++
                                                 maybe [] toContents h ++ maybe [] toContents i ++
                                                 toContents j ++ maybe [] toContents k ++
                                                 concatMap toContents l)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Article"]
        ; interior e $ return (Article (fromAttrs as))
                       `apply` parseContents `apply` parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` optional parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` parseContents `apply` optional parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <Article>, "++)
instance XmlAttributes Article_Attrs where
    fromAttrs as =
        Article_Attrs
          { articlePubModel = definiteA fromAttrToTyp "Article" "PubModel" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "PubModel" (articlePubModel v)
        ]

instance XmlAttrType Article_PubModel where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Print" = Just Article_PubModel_Print
            translate "Print-Electronic" = Just Article_PubModel_Print_Electronic
            translate "Electronic" = Just Article_PubModel_Electronic
            translate "Electronic-Print" = Just Article_PubModel_Electronic_Print
            translate _ = Nothing
    toAttrFrTyp n Article_PubModel_Print = Just (N n, str2attr "Print")
    toAttrFrTyp n Article_PubModel_Print_Electronic = Just (N n, str2attr "Print-Electronic")
    toAttrFrTyp n Article_PubModel_Electronic = Just (N n, str2attr "Electronic")
    toAttrFrTyp n Article_PubModel_Electronic_Print = Just (N n, str2attr "Electronic-Print")

instance HTypeable ArticleDate where
    toHType x = Defined "ArticleDate" [] []
instance XmlContent ArticleDate where
    toContents (ArticleDate as a b c) =
        [CElem (Elem (N "ArticleDate") (toAttrs as) (toContents a ++
                                                     toContents b ++ toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ArticleDate"]
        ; interior e $ return (ArticleDate (fromAttrs as))
                       `apply` parseContents `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <ArticleDate>, "++)
instance XmlAttributes ArticleDate_Attrs where
    fromAttrs as =
        ArticleDate_Attrs
          { articleDateDateType = defaultA fromAttrToStr "Electronic" "DateType" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "DateType" (articleDateDateType v)
        ]

instance HTypeable ArticleTitle where
    toHType x = Defined "ArticleTitle" [] []
instance XmlContent ArticleTitle where
    toContents (ArticleTitle a) =
        [CElem (Elem (N "ArticleTitle") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ArticleTitle"]
        ; interior e $ return (ArticleTitle)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ArticleTitle>, "++)

instance HTypeable Author where
    toHType x = Defined "Author" [] []
instance XmlContent Author where
    toContents (AuthorLastName_ForeName_Initials_Suffix_NameID as a) =
        [CElem (Elem (N "Author") (toAttrs as) (toContents a) ) ()]
    toContents (AuthorCollectiveName_NameID as a) =
        [CElem (Elem (N "Author") (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["Author"]
        ; interior e $ oneOf
            [ return (AuthorLastName_ForeName_Initials_Suffix_NameID (fromAttrs as))
              `apply` parseContents
            , return (AuthorCollectiveName_NameID (fromAttrs as))
              `apply` parseContents
            ] `adjustErr` ("in <Author>, "++)
        }
instance XmlAttributes Author_Attrs where
    fromAttrs as =
        Author_Attrs
          { authorValidYN = defaultA fromAttrToTyp Author_ValidYN_Y "ValidYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "ValidYN" (authorValidYN v)
        ]

instance XmlAttrType Author_ValidYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just Author_ValidYN_Y
            translate "N" = Just Author_ValidYN_N
            translate _ = Nothing
    toAttrFrTyp n Author_ValidYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n Author_ValidYN_N = Just (N n, str2attr "N")

instance HTypeable AuthorList where
    toHType x = Defined "AuthorList" [] []
instance XmlContent AuthorList where
    toContents (AuthorList as a) =
        [CElem (Elem (N "AuthorList") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["AuthorList"]
        ; interior e $ return (AuthorList (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <AuthorList>, "++)
instance XmlAttributes AuthorList_Attrs where
    fromAttrs as =
        AuthorList_Attrs
          { authorListCompleteYN = defaultA fromAttrToTyp AuthorList_CompleteYN_Y "CompleteYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "CompleteYN" (authorListCompleteYN v)
        ]

instance XmlAttrType AuthorList_CompleteYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just AuthorList_CompleteYN_Y
            translate "N" = Just AuthorList_CompleteYN_N
            translate _ = Nothing
    toAttrFrTyp n AuthorList_CompleteYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n AuthorList_CompleteYN_N = Just (N n, str2attr "N")

instance HTypeable Chemical where
    toHType x = Defined "Chemical" [] []
instance XmlContent Chemical where
    toContents (Chemical a b) =
        [CElem (Elem (N "Chemical") [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Chemical"]
        ; interior e $ return (Chemical) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Chemical>, "++)

instance HTypeable ChemicalList where
    toHType x = Defined "ChemicalList" [] []
instance XmlContent ChemicalList where
    toContents (ChemicalList a) =
        [CElem (Elem (N "ChemicalList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ChemicalList"]
        ; interior e $ return (ChemicalList) `apply` parseContents
        } `adjustErr` ("in <ChemicalList>, "++)

instance HTypeable CitationSubset where
    toHType x = Defined "CitationSubset" [] []
instance XmlContent CitationSubset where
    toContents (CitationSubset a) =
        [CElem (Elem (N "CitationSubset") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["CitationSubset"]
        ; interior e $ return (CitationSubset)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <CitationSubset>, "++)

instance HTypeable CollectiveName where
    toHType x = Defined "CollectiveName" [] []
instance XmlContent CollectiveName where
    toContents (CollectiveName a) =
        [CElem (Elem (N "CollectiveName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["CollectiveName"]
        ; interior e $ return (CollectiveName)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <CollectiveName>, "++)

instance HTypeable CommentsCorrections where
    toHType x = Defined "CommentsCorrections" [] []
instance XmlContent CommentsCorrections where
    toContents (CommentsCorrections as a b c) =
        [CElem (Elem (N "CommentsCorrections") (toAttrs as) (toContents a
                                                             ++ maybe [] toContents b ++
                                                             maybe [] toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["CommentsCorrections"]
        ; interior e $ return (CommentsCorrections (fromAttrs as))
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <CommentsCorrections>, "++)
instance XmlAttributes CommentsCorrections_Attrs where
    fromAttrs as =
        CommentsCorrections_Attrs
          { commentsCorrectionsRefType = definiteA fromAttrToTyp "CommentsCorrections" "RefType" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "RefType" (commentsCorrectionsRefType v)
        ]

instance XmlAttrType CommentsCorrections_RefType where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "CommentOn" = Just CommentsCorrections_RefType_CommentOn
            translate "CommentIn" = Just CommentsCorrections_RefType_CommentIn
            translate "ErratumIn" = Just CommentsCorrections_RefType_ErratumIn
            translate "ErratumFor" = Just CommentsCorrections_RefType_ErratumFor
            translate "PartialRetractionIn" = Just CommentsCorrections_RefType_PartialRetractionIn
            translate "PartialRetractionOf" = Just CommentsCorrections_RefType_PartialRetractionOf
            translate "RepublishedFrom" = Just CommentsCorrections_RefType_RepublishedFrom
            translate "RepublishedIn" = Just CommentsCorrections_RefType_RepublishedIn
            translate "RetractionOf" = Just CommentsCorrections_RefType_RetractionOf
            translate "RetractionIn" = Just CommentsCorrections_RefType_RetractionIn
            translate "UpdateIn" = Just CommentsCorrections_RefType_UpdateIn
            translate "UpdateOf" = Just CommentsCorrections_RefType_UpdateOf
            translate "SummaryForPatientsIn" = Just CommentsCorrections_RefType_SummaryForPatientsIn
            translate "OriginalReportIn" = Just CommentsCorrections_RefType_OriginalReportIn
            translate "ReprintOf" = Just CommentsCorrections_RefType_ReprintOf
            translate "ReprintIn" = Just CommentsCorrections_RefType_ReprintIn
            translate "Cites" = Just CommentsCorrections_RefType_Cites
            translate _ = Nothing
    toAttrFrTyp n CommentsCorrections_RefType_CommentOn = Just (N n, str2attr "CommentOn")
    toAttrFrTyp n CommentsCorrections_RefType_CommentIn = Just (N n, str2attr "CommentIn")
    toAttrFrTyp n CommentsCorrections_RefType_ErratumIn = Just (N n, str2attr "ErratumIn")
    toAttrFrTyp n CommentsCorrections_RefType_ErratumFor = Just (N n, str2attr "ErratumFor")
    toAttrFrTyp n CommentsCorrections_RefType_PartialRetractionIn = Just (N n, str2attr "PartialRetractionIn")
    toAttrFrTyp n CommentsCorrections_RefType_PartialRetractionOf = Just (N n, str2attr "PartialRetractionOf")
    toAttrFrTyp n CommentsCorrections_RefType_RepublishedFrom = Just (N n, str2attr "RepublishedFrom")
    toAttrFrTyp n CommentsCorrections_RefType_RepublishedIn = Just (N n, str2attr "RepublishedIn")
    toAttrFrTyp n CommentsCorrections_RefType_RetractionOf = Just (N n, str2attr "RetractionOf")
    toAttrFrTyp n CommentsCorrections_RefType_RetractionIn = Just (N n, str2attr "RetractionIn")
    toAttrFrTyp n CommentsCorrections_RefType_UpdateIn = Just (N n, str2attr "UpdateIn")
    toAttrFrTyp n CommentsCorrections_RefType_UpdateOf = Just (N n, str2attr "UpdateOf")
    toAttrFrTyp n CommentsCorrections_RefType_SummaryForPatientsIn = Just (N n, str2attr "SummaryForPatientsIn")
    toAttrFrTyp n CommentsCorrections_RefType_OriginalReportIn = Just (N n, str2attr "OriginalReportIn")
    toAttrFrTyp n CommentsCorrections_RefType_ReprintOf = Just (N n, str2attr "ReprintOf")
    toAttrFrTyp n CommentsCorrections_RefType_ReprintIn = Just (N n, str2attr "ReprintIn")
    toAttrFrTyp n CommentsCorrections_RefType_Cites = Just (N n, str2attr "Cites")

instance HTypeable CommentsCorrectionsList where
    toHType x = Defined "CommentsCorrectionsList" [] []
instance XmlContent CommentsCorrectionsList where
    toContents (CommentsCorrectionsList a) =
        [CElem (Elem (N "CommentsCorrectionsList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["CommentsCorrectionsList"]
        ; interior e $ return (CommentsCorrectionsList)
                       `apply` parseContents
        } `adjustErr` ("in <CommentsCorrectionsList>, "++)

instance HTypeable CopyrightInformation where
    toHType x = Defined "CopyrightInformation" [] []
instance XmlContent CopyrightInformation where
    toContents (CopyrightInformation a) =
        [CElem (Elem (N "CopyrightInformation") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["CopyrightInformation"]
        ; interior e $ return (CopyrightInformation)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <CopyrightInformation>, "++)

instance HTypeable Country where
    toHType x = Defined "Country" [] []
instance XmlContent Country where
    toContents (Country a) =
        [CElem (Elem (N "Country") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Country"]
        ; interior e $ return (Country) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Country>, "++)

instance HTypeable DataBank where
    toHType x = Defined "DataBank" [] []
instance XmlContent DataBank where
    toContents (DataBank a b) =
        [CElem (Elem (N "DataBank") [] (toContents a ++
                                        maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DataBank"]
        ; interior e $ return (DataBank) `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <DataBank>, "++)

instance HTypeable DataBankList where
    toHType x = Defined "DataBankList" [] []
instance XmlContent DataBankList where
    toContents (DataBankList as a) =
        [CElem (Elem (N "DataBankList") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["DataBankList"]
        ; interior e $ return (DataBankList (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <DataBankList>, "++)
instance XmlAttributes DataBankList_Attrs where
    fromAttrs as =
        DataBankList_Attrs
          { dataBankListCompleteYN = defaultA fromAttrToTyp DataBankList_CompleteYN_Y "CompleteYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "CompleteYN" (dataBankListCompleteYN v)
        ]

instance XmlAttrType DataBankList_CompleteYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just DataBankList_CompleteYN_Y
            translate "N" = Just DataBankList_CompleteYN_N
            translate _ = Nothing
    toAttrFrTyp n DataBankList_CompleteYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n DataBankList_CompleteYN_N = Just (N n, str2attr "N")

instance HTypeable DataBankName where
    toHType x = Defined "DataBankName" [] []
instance XmlContent DataBankName where
    toContents (DataBankName a) =
        [CElem (Elem (N "DataBankName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DataBankName"]
        ; interior e $ return (DataBankName)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <DataBankName>, "++)

instance HTypeable DateCompleted where
    toHType x = Defined "DateCompleted" [] []
instance XmlContent DateCompleted where
    toContents (DateCompleted a b c) =
        [CElem (Elem (N "DateCompleted") [] (toContents a ++ toContents b
                                             ++ toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DateCompleted"]
        ; interior e $ return (DateCompleted) `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <DateCompleted>, "++)

instance HTypeable DateCreated where
    toHType x = Defined "DateCreated" [] []
instance XmlContent DateCreated where
    toContents (DateCreated a b c) =
        [CElem (Elem (N "DateCreated") [] (toContents a ++ toContents b ++
                                           toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DateCreated"]
        ; interior e $ return (DateCreated) `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <DateCreated>, "++)

instance HTypeable DateRevised where
    toHType x = Defined "DateRevised" [] []
instance XmlContent DateRevised where
    toContents (DateRevised a b c) =
        [CElem (Elem (N "DateRevised") [] (toContents a ++ toContents b ++
                                           toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DateRevised"]
        ; interior e $ return (DateRevised) `apply` parseContents
                       `apply` parseContents `apply` parseContents
        } `adjustErr` ("in <DateRevised>, "++)

instance HTypeable Day where
    toHType x = Defined "Day" [] []
instance XmlContent Day where
    toContents (Day a) =
        [CElem (Elem (N "Day") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Day"]
        ; interior e $ return (Day) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Day>, "++)

instance HTypeable DescriptorName where
    toHType x = Defined "DescriptorName" [] []
instance XmlContent DescriptorName where
    toContents (DescriptorName as a) =
        [CElem (Elem (N "DescriptorName") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["DescriptorName"]
        ; interior e $ return (DescriptorName (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <DescriptorName>, "++)
instance XmlAttributes DescriptorName_Attrs where
    fromAttrs as =
        DescriptorName_Attrs
          { descriptorNameMajorTopicYN = defaultA fromAttrToTyp DescriptorName_MajorTopicYN_N "MajorTopicYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "MajorTopicYN" (descriptorNameMajorTopicYN v)
        ]

instance XmlAttrType DescriptorName_MajorTopicYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just DescriptorName_MajorTopicYN_Y
            translate "N" = Just DescriptorName_MajorTopicYN_N
            translate _ = Nothing
    toAttrFrTyp n DescriptorName_MajorTopicYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n DescriptorName_MajorTopicYN_N = Just (N n, str2attr "N")

instance HTypeable ELocationID where
    toHType x = Defined "ELocationID" [] []
instance XmlContent ELocationID where
    toContents (ELocationID as a) =
        [CElem (Elem (N "ELocationID") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ELocationID"]
        ; interior e $ return (ELocationID (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ELocationID>, "++)
instance XmlAttributes ELocationID_Attrs where
    fromAttrs as =
        ELocationID_Attrs
          { eLocationIDEIdType = definiteA fromAttrToTyp "ELocationID" "EIdType" as
          , eLocationIDValidYN = defaultA fromAttrToTyp ELocationID_ValidYN_Y "ValidYN" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "EIdType" (eLocationIDEIdType v)
        , defaultToAttr toAttrFrTyp "ValidYN" (eLocationIDValidYN v)
        ]

instance XmlAttrType ELocationID_EIdType where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "doi" = Just ELocationID_EIdType_doi
            translate "pii" = Just ELocationID_EIdType_pii
            translate _ = Nothing
    toAttrFrTyp n ELocationID_EIdType_doi = Just (N n, str2attr "doi")
    toAttrFrTyp n ELocationID_EIdType_pii = Just (N n, str2attr "pii")

instance XmlAttrType ELocationID_ValidYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just ELocationID_ValidYN_Y
            translate "N" = Just ELocationID_ValidYN_N
            translate _ = Nothing
    toAttrFrTyp n ELocationID_ValidYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n ELocationID_ValidYN_N = Just (N n, str2attr "N")

instance HTypeable EndPage where
    toHType x = Defined "EndPage" [] []
instance XmlContent EndPage where
    toContents (EndPage a) =
        [CElem (Elem (N "EndPage") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["EndPage"]
        ; interior e $ return (EndPage) `apply` (text `onFail` return "")
        } `adjustErr` ("in <EndPage>, "++)

instance HTypeable ForeName where
    toHType x = Defined "ForeName" [] []
instance XmlContent ForeName where
    toContents (ForeName a) =
        [CElem (Elem (N "ForeName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ForeName"]
        ; interior e $ return (ForeName) `apply` (text `onFail` return "")
        } `adjustErr` ("in <ForeName>, "++)

instance HTypeable GeneSymbol where
    toHType x = Defined "GeneSymbol" [] []
instance XmlContent GeneSymbol where
    toContents (GeneSymbol a) =
        [CElem (Elem (N "GeneSymbol") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["GeneSymbol"]
        ; interior e $ return (GeneSymbol)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <GeneSymbol>, "++)

instance HTypeable GeneSymbolList where
    toHType x = Defined "GeneSymbolList" [] []
instance XmlContent GeneSymbolList where
    toContents (GeneSymbolList a) =
        [CElem (Elem (N "GeneSymbolList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["GeneSymbolList"]
        ; interior e $ return (GeneSymbolList) `apply` parseContents
        } `adjustErr` ("in <GeneSymbolList>, "++)

instance HTypeable GeneralNote where
    toHType x = Defined "GeneralNote" [] []
instance XmlContent GeneralNote where
    toContents (GeneralNote as a) =
        [CElem (Elem (N "GeneralNote") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["GeneralNote"]
        ; interior e $ return (GeneralNote (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <GeneralNote>, "++)
instance XmlAttributes GeneralNote_Attrs where
    fromAttrs as =
        GeneralNote_Attrs
          { generalNoteOwner = defaultA fromAttrToTyp GeneralNote_Owner_NLM "Owner" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "Owner" (generalNoteOwner v)
        ]

instance XmlAttrType GeneralNote_Owner where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "NLM" = Just GeneralNote_Owner_NLM
            translate "NASA" = Just GeneralNote_Owner_NASA
            translate "PIP" = Just GeneralNote_Owner_PIP
            translate "KIE" = Just GeneralNote_Owner_KIE
            translate "HSR" = Just GeneralNote_Owner_HSR
            translate "HMD" = Just GeneralNote_Owner_HMD
            translate _ = Nothing
    toAttrFrTyp n GeneralNote_Owner_NLM = Just (N n, str2attr "NLM")
    toAttrFrTyp n GeneralNote_Owner_NASA = Just (N n, str2attr "NASA")
    toAttrFrTyp n GeneralNote_Owner_PIP = Just (N n, str2attr "PIP")
    toAttrFrTyp n GeneralNote_Owner_KIE = Just (N n, str2attr "KIE")
    toAttrFrTyp n GeneralNote_Owner_HSR = Just (N n, str2attr "HSR")
    toAttrFrTyp n GeneralNote_Owner_HMD = Just (N n, str2attr "HMD")

instance HTypeable Grant where
    toHType x = Defined "Grant" [] []
instance XmlContent Grant where
    toContents (Grant a b c d) =
        [CElem (Elem (N "Grant") [] (maybe [] toContents a ++
                                     maybe [] toContents b ++ toContents c ++ toContents d)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Grant"]
        ; interior e $ return (Grant) `apply` optional parseContents
                       `apply` optional parseContents `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Grant>, "++)

instance HTypeable GrantID where
    toHType x = Defined "GrantID" [] []
instance XmlContent GrantID where
    toContents (GrantID a) =
        [CElem (Elem (N "GrantID") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["GrantID"]
        ; interior e $ return (GrantID) `apply` (text `onFail` return "")
        } `adjustErr` ("in <GrantID>, "++)

instance HTypeable GrantList where
    toHType x = Defined "GrantList" [] []
instance XmlContent GrantList where
    toContents (GrantList as a) =
        [CElem (Elem (N "GrantList") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["GrantList"]
        ; interior e $ return (GrantList (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <GrantList>, "++)
instance XmlAttributes GrantList_Attrs where
    fromAttrs as =
        GrantList_Attrs
          { grantListCompleteYN = defaultA fromAttrToTyp GrantList_CompleteYN_Y "CompleteYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "CompleteYN" (grantListCompleteYN v)
        ]

instance XmlAttrType GrantList_CompleteYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just GrantList_CompleteYN_Y
            translate "N" = Just GrantList_CompleteYN_N
            translate _ = Nothing
    toAttrFrTyp n GrantList_CompleteYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n GrantList_CompleteYN_N = Just (N n, str2attr "N")

instance HTypeable ISOAbbreviation where
    toHType x = Defined "ISOAbbreviation" [] []
instance XmlContent ISOAbbreviation where
    toContents (ISOAbbreviation a) =
        [CElem (Elem (N "ISOAbbreviation") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ISOAbbreviation"]
        ; interior e $ return (ISOAbbreviation)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ISOAbbreviation>, "++)

instance HTypeable ISSN where
    toHType x = Defined "ISSN" [] []
instance XmlContent ISSN where
    toContents (ISSN as a) =
        [CElem (Elem (N "ISSN") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ISSN"]
        ; interior e $ return (ISSN (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ISSN>, "++)
instance XmlAttributes ISSN_Attrs where
    fromAttrs as =
        ISSN_Attrs
          { iSSNIssnType = definiteA fromAttrToTyp "ISSN" "IssnType" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "IssnType" (iSSNIssnType v)
        ]

instance XmlAttrType ISSN_IssnType where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Electronic" = Just ISSN_IssnType_Electronic
            translate "Print" = Just ISSN_IssnType_Print
            translate _ = Nothing
    toAttrFrTyp n ISSN_IssnType_Electronic = Just (N n, str2attr "Electronic")
    toAttrFrTyp n ISSN_IssnType_Print = Just (N n, str2attr "Print")

instance HTypeable ISSNLinking where
    toHType x = Defined "ISSNLinking" [] []
instance XmlContent ISSNLinking where
    toContents (ISSNLinking a) =
        [CElem (Elem (N "ISSNLinking") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ISSNLinking"]
        ; interior e $ return (ISSNLinking)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ISSNLinking>, "++)

instance HTypeable Initials where
    toHType x = Defined "Initials" [] []
instance XmlContent Initials where
    toContents (Initials a) =
        [CElem (Elem (N "Initials") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Initials"]
        ; interior e $ return (Initials) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Initials>, "++)

instance HTypeable Investigator where
    toHType x = Defined "Investigator" [] []
instance XmlContent Investigator where
    toContents (Investigator as a b c d e f) =
        [CElem (Elem (N "Investigator") (toAttrs as) (toContents a ++
                                                      maybe [] toContents b ++ maybe [] toContents c
                                                      ++ maybe [] toContents d ++
                                                      concatMap toContents e ++
                                                      maybe [] toContents f)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Investigator"]
        ; interior e $ return (Investigator (fromAttrs as))
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` many parseContents `apply` optional parseContents
        } `adjustErr` ("in <Investigator>, "++)
instance XmlAttributes Investigator_Attrs where
    fromAttrs as =
        Investigator_Attrs
          { investigatorValidYN = defaultA fromAttrToTyp Investigator_ValidYN_Y "ValidYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "ValidYN" (investigatorValidYN v)
        ]

instance XmlAttrType Investigator_ValidYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just Investigator_ValidYN_Y
            translate "N" = Just Investigator_ValidYN_N
            translate _ = Nothing
    toAttrFrTyp n Investigator_ValidYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n Investigator_ValidYN_N = Just (N n, str2attr "N")

instance HTypeable InvestigatorList where
    toHType x = Defined "InvestigatorList" [] []
instance XmlContent InvestigatorList where
    toContents (InvestigatorList a) =
        [CElem (Elem (N "InvestigatorList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["InvestigatorList"]
        ; interior e $ return (InvestigatorList) `apply` parseContents
        } `adjustErr` ("in <InvestigatorList>, "++)

instance HTypeable Issue where
    toHType x = Defined "Issue" [] []
instance XmlContent Issue where
    toContents (Issue a) =
        [CElem (Elem (N "Issue") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Issue"]
        ; interior e $ return (Issue) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Issue>, "++)

instance HTypeable Journal where
    toHType x = Defined "Journal" [] []
instance XmlContent Journal where
    toContents (Journal a b c d) =
        [CElem (Elem (N "Journal") [] (maybe [] toContents a ++
                                       toContents b ++ maybe [] toContents c ++
                                       maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Journal"]
        ; interior e $ return (Journal) `apply` optional parseContents
                       `apply` parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <Journal>, "++)

instance HTypeable JournalIssue where
    toHType x = Defined "JournalIssue" [] []
instance XmlContent JournalIssue where
    toContents (JournalIssue as a b c) =
        [CElem (Elem (N "JournalIssue") (toAttrs as) (maybe [] toContents a
                                                      ++ maybe [] toContents b ++ toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["JournalIssue"]
        ; interior e $ return (JournalIssue (fromAttrs as))
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` parseContents
        } `adjustErr` ("in <JournalIssue>, "++)
instance XmlAttributes JournalIssue_Attrs where
    fromAttrs as =
        JournalIssue_Attrs
          { journalIssueCitedMedium = definiteA fromAttrToTyp "JournalIssue" "CitedMedium" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "CitedMedium" (journalIssueCitedMedium v)
        ]

instance XmlAttrType JournalIssue_CitedMedium where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Internet" = Just JournalIssue_CitedMedium_Internet
            translate "Print" = Just JournalIssue_CitedMedium_Print
            translate _ = Nothing
    toAttrFrTyp n JournalIssue_CitedMedium_Internet = Just (N n, str2attr "Internet")
    toAttrFrTyp n JournalIssue_CitedMedium_Print = Just (N n, str2attr "Print")

instance HTypeable Keyword where
    toHType x = Defined "Keyword" [] []
instance XmlContent Keyword where
    toContents (Keyword as a) =
        [CElem (Elem (N "Keyword") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Keyword"]
        ; interior e $ return (Keyword (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <Keyword>, "++)
instance XmlAttributes Keyword_Attrs where
    fromAttrs as =
        Keyword_Attrs
          { keywordMajorTopicYN = defaultA fromAttrToTyp Keyword_MajorTopicYN_N "MajorTopicYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "MajorTopicYN" (keywordMajorTopicYN v)
        ]

instance XmlAttrType Keyword_MajorTopicYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just Keyword_MajorTopicYN_Y
            translate "N" = Just Keyword_MajorTopicYN_N
            translate _ = Nothing
    toAttrFrTyp n Keyword_MajorTopicYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n Keyword_MajorTopicYN_N = Just (N n, str2attr "N")

instance HTypeable KeywordList where
    toHType x = Defined "KeywordList" [] []
instance XmlContent KeywordList where
    toContents (KeywordList as a) =
        [CElem (Elem (N "KeywordList") (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["KeywordList"]
        ; interior e $ return (KeywordList (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <KeywordList>, "++)
instance XmlAttributes KeywordList_Attrs where
    fromAttrs as =
        KeywordList_Attrs
          { keywordListOwner = defaultA fromAttrToTyp KeywordList_Owner_NLM "Owner" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "Owner" (keywordListOwner v)
        ]

instance XmlAttrType KeywordList_Owner where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "NLM" = Just KeywordList_Owner_NLM
            translate "NASA" = Just KeywordList_Owner_NASA
            translate "PIP" = Just KeywordList_Owner_PIP
            translate "KIE" = Just KeywordList_Owner_KIE
            translate "NOTNLM" = Just KeywordList_Owner_NOTNLM
            translate _ = Nothing
    toAttrFrTyp n KeywordList_Owner_NLM = Just (N n, str2attr "NLM")
    toAttrFrTyp n KeywordList_Owner_NASA = Just (N n, str2attr "NASA")
    toAttrFrTyp n KeywordList_Owner_PIP = Just (N n, str2attr "PIP")
    toAttrFrTyp n KeywordList_Owner_KIE = Just (N n, str2attr "KIE")
    toAttrFrTyp n KeywordList_Owner_NOTNLM = Just (N n, str2attr "NOTNLM")

instance HTypeable Language where
    toHType x = Defined "Language" [] []
instance XmlContent Language where
    toContents (Language a) =
        [CElem (Elem (N "Language") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Language"]
        ; interior e $ return (Language) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Language>, "++)

instance HTypeable LastName where
    toHType x = Defined "LastName" [] []
instance XmlContent LastName where
    toContents (LastName a) =
        [CElem (Elem (N "LastName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["LastName"]
        ; interior e $ return (LastName) `apply` (text `onFail` return "")
        } `adjustErr` ("in <LastName>, "++)

instance HTypeable MedlineDate where
    toHType x = Defined "MedlineDate" [] []
instance XmlContent MedlineDate where
    toContents (MedlineDate a) =
        [CElem (Elem (N "MedlineDate") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MedlineDate"]
        ; interior e $ return (MedlineDate)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <MedlineDate>, "++)

instance HTypeable MedlineJournalInfo where
    toHType x = Defined "MedlineJournalInfo" [] []
instance XmlContent MedlineJournalInfo where
    toContents (MedlineJournalInfo a b c d) =
        [CElem (Elem (N "MedlineJournalInfo") [] (maybe [] toContents a ++
                                                  toContents b ++ maybe [] toContents c ++
                                                  maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MedlineJournalInfo"]
        ; interior e $ return (MedlineJournalInfo)
                       `apply` optional parseContents `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
        } `adjustErr` ("in <MedlineJournalInfo>, "++)

instance HTypeable MedlinePgn where
    toHType x = Defined "MedlinePgn" [] []
instance XmlContent MedlinePgn where
    toContents (MedlinePgn a) =
        [CElem (Elem (N "MedlinePgn") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MedlinePgn"]
        ; interior e $ return (MedlinePgn)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <MedlinePgn>, "++)

instance HTypeable MedlineTA where
    toHType x = Defined "MedlineTA" [] []
instance XmlContent MedlineTA where
    toContents (MedlineTA a) =
        [CElem (Elem (N "MedlineTA") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MedlineTA"]
        ; interior e $ return (MedlineTA) `apply` (text `onFail` return "")
        } `adjustErr` ("in <MedlineTA>, "++)

instance HTypeable MeshHeading where
    toHType x = Defined "MeshHeading" [] []
instance XmlContent MeshHeading where
    toContents (MeshHeading a b) =
        [CElem (Elem (N "MeshHeading") [] (toContents a ++
                                           concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MeshHeading"]
        ; interior e $ return (MeshHeading) `apply` parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <MeshHeading>, "++)

instance HTypeable MeshHeadingList where
    toHType x = Defined "MeshHeadingList" [] []
instance XmlContent MeshHeadingList where
    toContents (MeshHeadingList a) =
        [CElem (Elem (N "MeshHeadingList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["MeshHeadingList"]
        ; interior e $ return (MeshHeadingList) `apply` parseContents
        } `adjustErr` ("in <MeshHeadingList>, "++)

instance HTypeable Month where
    toHType x = Defined "Month" [] []
instance XmlContent Month where
    toContents (Month a) =
        [CElem (Elem (N "Month") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Month"]
        ; interior e $ return (Month) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Month>, "++)

instance HTypeable NameID where
    toHType x = Defined "NameID" [] []
instance XmlContent NameID where
    toContents (NameID as a) =
        [CElem (Elem (N "NameID") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["NameID"]
        ; interior e $ return (NameID (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <NameID>, "++)
instance XmlAttributes NameID_Attrs where
    fromAttrs as =
        NameID_Attrs
          { nameIDSource = definiteA fromAttrToTyp "NameID" "Source" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "Source" (nameIDSource v)
        ]

instance XmlAttrType NameID_Source where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "NCBI" = Just NameID_Source_NCBI
            translate "Publisher" = Just NameID_Source_Publisher
            translate "NISO" = Just NameID_Source_NISO
            translate "ISO" = Just NameID_Source_ISO
            translate _ = Nothing
    toAttrFrTyp n NameID_Source_NCBI = Just (N n, str2attr "NCBI")
    toAttrFrTyp n NameID_Source_Publisher = Just (N n, str2attr "Publisher")
    toAttrFrTyp n NameID_Source_NISO = Just (N n, str2attr "NISO")
    toAttrFrTyp n NameID_Source_ISO = Just (N n, str2attr "ISO")

instance HTypeable NameOfSubstance where
    toHType x = Defined "NameOfSubstance" [] []
instance XmlContent NameOfSubstance where
    toContents (NameOfSubstance a) =
        [CElem (Elem (N "NameOfSubstance") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["NameOfSubstance"]
        ; interior e $ return (NameOfSubstance)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <NameOfSubstance>, "++)

instance HTypeable NlmUniqueID where
    toHType x = Defined "NlmUniqueID" [] []
instance XmlContent NlmUniqueID where
    toContents (NlmUniqueID a) =
        [CElem (Elem (N "NlmUniqueID") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["NlmUniqueID"]
        ; interior e $ return (NlmUniqueID)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <NlmUniqueID>, "++)

instance HTypeable Note where
    toHType x = Defined "Note" [] []
instance XmlContent Note where
    toContents (Note a) =
        [CElem (Elem (N "Note") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Note"]
        ; interior e $ return (Note) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Note>, "++)

instance HTypeable NumberOfReferences where
    toHType x = Defined "NumberOfReferences" [] []
instance XmlContent NumberOfReferences where
    toContents (NumberOfReferences a) =
        [CElem (Elem (N "NumberOfReferences") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["NumberOfReferences"]
        ; interior e $ return (NumberOfReferences)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <NumberOfReferences>, "++)

instance HTypeable OtherAbstract where
    toHType x = Defined "OtherAbstract" [] []
instance XmlContent OtherAbstract where
    toContents (OtherAbstract as a b) =
        [CElem (Elem (N "OtherAbstract") (toAttrs as) (toContents a ++
                                                       maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["OtherAbstract"]
        ; interior e $ return (OtherAbstract (fromAttrs as))
                       `apply` parseContents `apply` optional parseContents
        } `adjustErr` ("in <OtherAbstract>, "++)
instance XmlAttributes OtherAbstract_Attrs where
    fromAttrs as =
        OtherAbstract_Attrs
          { otherAbstractType = definiteA fromAttrToTyp "OtherAbstract" "Type" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "Type" (otherAbstractType v)
        ]

instance XmlAttrType OtherAbstract_Type where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "AAMC" = Just OtherAbstract_Type_AAMC
            translate "AIDS" = Just OtherAbstract_Type_AIDS
            translate "KIE" = Just OtherAbstract_Type_KIE
            translate "PIP" = Just OtherAbstract_Type_PIP
            translate "NASA" = Just OtherAbstract_Type_NASA
            translate "Publisher" = Just OtherAbstract_Type_Publisher
            translate _ = Nothing
    toAttrFrTyp n OtherAbstract_Type_AAMC = Just (N n, str2attr "AAMC")
    toAttrFrTyp n OtherAbstract_Type_AIDS = Just (N n, str2attr "AIDS")
    toAttrFrTyp n OtherAbstract_Type_KIE = Just (N n, str2attr "KIE")
    toAttrFrTyp n OtherAbstract_Type_PIP = Just (N n, str2attr "PIP")
    toAttrFrTyp n OtherAbstract_Type_NASA = Just (N n, str2attr "NASA")
    toAttrFrTyp n OtherAbstract_Type_Publisher = Just (N n, str2attr "Publisher")

instance HTypeable OtherID where
    toHType x = Defined "OtherID" [] []
instance XmlContent OtherID where
    toContents (OtherID as a) =
        [CElem (Elem (N "OtherID") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["OtherID"]
        ; interior e $ return (OtherID (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <OtherID>, "++)
instance XmlAttributes OtherID_Attrs where
    fromAttrs as =
        OtherID_Attrs
          { otherIDSource = definiteA fromAttrToTyp "OtherID" "Source" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "Source" (otherIDSource v)
        ]

instance XmlAttrType OtherID_Source where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "NASA" = Just OtherID_Source_NASA
            translate "KIE" = Just OtherID_Source_KIE
            translate "PIP" = Just OtherID_Source_PIP
            translate "POP" = Just OtherID_Source_POP
            translate "ARPL" = Just OtherID_Source_ARPL
            translate "CPC" = Just OtherID_Source_CPC
            translate "IND" = Just OtherID_Source_IND
            translate "CPFH" = Just OtherID_Source_CPFH
            translate "CLML" = Just OtherID_Source_CLML
            translate "NRCBL" = Just OtherID_Source_NRCBL
            translate "NLM" = Just OtherID_Source_NLM
            translate _ = Nothing
    toAttrFrTyp n OtherID_Source_NASA = Just (N n, str2attr "NASA")
    toAttrFrTyp n OtherID_Source_KIE = Just (N n, str2attr "KIE")
    toAttrFrTyp n OtherID_Source_PIP = Just (N n, str2attr "PIP")
    toAttrFrTyp n OtherID_Source_POP = Just (N n, str2attr "POP")
    toAttrFrTyp n OtherID_Source_ARPL = Just (N n, str2attr "ARPL")
    toAttrFrTyp n OtherID_Source_CPC = Just (N n, str2attr "CPC")
    toAttrFrTyp n OtherID_Source_IND = Just (N n, str2attr "IND")
    toAttrFrTyp n OtherID_Source_CPFH = Just (N n, str2attr "CPFH")
    toAttrFrTyp n OtherID_Source_CLML = Just (N n, str2attr "CLML")
    toAttrFrTyp n OtherID_Source_NRCBL = Just (N n, str2attr "NRCBL")
    toAttrFrTyp n OtherID_Source_NLM = Just (N n, str2attr "NLM")

instance HTypeable PMID where
    toHType x = Defined "PMID" [] []
instance XmlContent PMID where
    toContents (PMID a) =
        [CElem (Elem (N "PMID") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PMID"]
        ; interior e $ return (PMID) `apply` (text `onFail` return "")
        } `adjustErr` ("in <PMID>, "++)

instance HTypeable Pagination where
    toHType x = Defined "Pagination" [] []
instance XmlContent Pagination where
    toContents (PaginationStartPage_EndPage_MedlinePgn a) =
        [CElem (Elem (N "Pagination") [] (toContents a) ) ()]
    toContents (PaginationMedlinePgn a) =
        [CElem (Elem (N "Pagination") [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["Pagination"]
        ; interior e $ oneOf
            [ return (PaginationStartPage_EndPage_MedlinePgn)
              `apply` parseContents
            , return (PaginationMedlinePgn) `apply` parseContents
            ] `adjustErr` ("in <Pagination>, "++)
        }

instance HTypeable PersonalNameSubject where
    toHType x = Defined "PersonalNameSubject" [] []
instance XmlContent PersonalNameSubject where
    toContents (PersonalNameSubject a b c d) =
        [CElem (Elem (N "PersonalNameSubject") [] (toContents a ++
                                                   maybe [] toContents b ++ maybe [] toContents c ++
                                                   maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PersonalNameSubject"]
        ; interior e $ return (PersonalNameSubject) `apply` parseContents
                       `apply` optional parseContents `apply` optional parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <PersonalNameSubject>, "++)

instance HTypeable PersonalNameSubjectList where
    toHType x = Defined "PersonalNameSubjectList" [] []
instance XmlContent PersonalNameSubjectList where
    toContents (PersonalNameSubjectList a) =
        [CElem (Elem (N "PersonalNameSubjectList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PersonalNameSubjectList"]
        ; interior e $ return (PersonalNameSubjectList)
                       `apply` parseContents
        } `adjustErr` ("in <PersonalNameSubjectList>, "++)

instance HTypeable PubDate where
    toHType x = Defined "PubDate" [] []
instance XmlContent PubDate where
    toContents (PubDateYear_Month_Day_Season a) =
        [CElem (Elem (N "PubDate") [] (toContents a) ) ()]
    toContents (PubDateMedlineDate a) =
        [CElem (Elem (N "PubDate") [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["PubDate"]
        ; interior e $ oneOf
            [ return (PubDateYear_Month_Day_Season) `apply` parseContents
            , return (PubDateMedlineDate) `apply` parseContents
            ] `adjustErr` ("in <PubDate>, "++)
        }

instance HTypeable PublicationType where
    toHType x = Defined "PublicationType" [] []
instance XmlContent PublicationType where
    toContents (PublicationType a) =
        [CElem (Elem (N "PublicationType") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PublicationType"]
        ; interior e $ return (PublicationType)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <PublicationType>, "++)

instance HTypeable PublicationTypeList where
    toHType x = Defined "PublicationTypeList" [] []
instance XmlContent PublicationTypeList where
    toContents (PublicationTypeList a) =
        [CElem (Elem (N "PublicationTypeList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PublicationTypeList"]
        ; interior e $ return (PublicationTypeList) `apply` parseContents
        } `adjustErr` ("in <PublicationTypeList>, "++)

instance HTypeable QualifierName where
    toHType x = Defined "QualifierName" [] []
instance XmlContent QualifierName where
    toContents (QualifierName as a) =
        [CElem (Elem (N "QualifierName") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["QualifierName"]
        ; interior e $ return (QualifierName (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <QualifierName>, "++)
instance XmlAttributes QualifierName_Attrs where
    fromAttrs as =
        QualifierName_Attrs
          { qualifierNameMajorTopicYN = defaultA fromAttrToTyp QualifierName_MajorTopicYN_N "MajorTopicYN" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "MajorTopicYN" (qualifierNameMajorTopicYN v)
        ]

instance XmlAttrType QualifierName_MajorTopicYN where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "Y" = Just QualifierName_MajorTopicYN_Y
            translate "N" = Just QualifierName_MajorTopicYN_N
            translate _ = Nothing
    toAttrFrTyp n QualifierName_MajorTopicYN_Y = Just (N n, str2attr "Y")
    toAttrFrTyp n QualifierName_MajorTopicYN_N = Just (N n, str2attr "N")

instance HTypeable RefSource where
    toHType x = Defined "RefSource" [] []
instance XmlContent RefSource where
    toContents (RefSource a) =
        [CElem (Elem (N "RefSource") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["RefSource"]
        ; interior e $ return (RefSource) `apply` (text `onFail` return "")
        } `adjustErr` ("in <RefSource>, "++)

instance HTypeable RegistryNumber where
    toHType x = Defined "RegistryNumber" [] []
instance XmlContent RegistryNumber where
    toContents (RegistryNumber a) =
        [CElem (Elem (N "RegistryNumber") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["RegistryNumber"]
        ; interior e $ return (RegistryNumber)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <RegistryNumber>, "++)

instance HTypeable Season where
    toHType x = Defined "Season" [] []
instance XmlContent Season where
    toContents (Season a) =
        [CElem (Elem (N "Season") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Season"]
        ; interior e $ return (Season) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Season>, "++)

instance HTypeable SpaceFlightMission where
    toHType x = Defined "SpaceFlightMission" [] []
instance XmlContent SpaceFlightMission where
    toContents (SpaceFlightMission a) =
        [CElem (Elem (N "SpaceFlightMission") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["SpaceFlightMission"]
        ; interior e $ return (SpaceFlightMission)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <SpaceFlightMission>, "++)

instance HTypeable StartPage where
    toHType x = Defined "StartPage" [] []
instance XmlContent StartPage where
    toContents (StartPage a) =
        [CElem (Elem (N "StartPage") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["StartPage"]
        ; interior e $ return (StartPage) `apply` (text `onFail` return "")
        } `adjustErr` ("in <StartPage>, "++)

instance HTypeable Suffix where
    toHType x = Defined "Suffix" [] []
instance XmlContent Suffix where
    toContents (Suffix a) =
        [CElem (Elem (N "Suffix") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Suffix"]
        ; interior e $ return (Suffix) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Suffix>, "++)

instance HTypeable Title where
    toHType x = Defined "Title" [] []
instance XmlContent Title where
    toContents (Title a) =
        [CElem (Elem (N "Title") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Title"]
        ; interior e $ return (Title) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Title>, "++)

instance HTypeable VernacularTitle where
    toHType x = Defined "VernacularTitle" [] []
instance XmlContent VernacularTitle where
    toContents (VernacularTitle a) =
        [CElem (Elem (N "VernacularTitle") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["VernacularTitle"]
        ; interior e $ return (VernacularTitle)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <VernacularTitle>, "++)

instance HTypeable Volume where
    toHType x = Defined "Volume" [] []
instance XmlContent Volume where
    toContents (Volume a) =
        [CElem (Elem (N "Volume") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Volume"]
        ; interior e $ return (Volume) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Volume>, "++)

instance HTypeable Year where
    toHType x = Defined "Year" [] []
instance XmlContent Year where
    toContents (Year a) =
        [CElem (Elem (N "Year") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Year"]
        ; interior e $ return (Year) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Year>, "++)

instance HTypeable DeleteCitation where
    toHType x = Defined "DeleteCitation" [] []
instance XmlContent DeleteCitation where
    toContents (DeleteCitation a) =
        [CElem (Elem (N "DeleteCitation") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["DeleteCitation"]
        ; interior e $ return (DeleteCitation) `apply` parseContents
        } `adjustErr` ("in <DeleteCitation>, "++)

instance HTypeable PubmedArticleSet where
    toHType x = Defined "PubmedArticleSet" [] []
instance XmlContent PubmedArticleSet where
    toContents (PubmedArticleSet a) =
        [CElem (Elem (N "PubmedArticleSet") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PubmedArticleSet"]
        ; interior e $ return (PubmedArticleSet) `apply` parseContents
        } `adjustErr` ("in <PubmedArticleSet>, "++)

instance HTypeable PubmedArticle where
    toHType x = Defined "PubmedArticle" [] []
instance XmlContent PubmedArticle where
    toContents (PubmedArticle a b) =
        [CElem (Elem (N "PubmedArticle") [] (toContents a ++
                                             maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PubmedArticle"]
        ; interior e $ return (PubmedArticle) `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <PubmedArticle>, "++)

instance HTypeable PubmedData where
    toHType x = Defined "PubmedData" [] []
instance XmlContent PubmedData where
    toContents (PubmedData a b c d) =
        [CElem (Elem (N "PubmedData") [] (maybe [] toContents a ++
                                          toContents b ++ toContents c ++
                                          maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PubmedData"]
        ; interior e $ return (PubmedData) `apply` optional parseContents
                       `apply` parseContents `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <PubmedData>, "++)

instance HTypeable PubMedPubDate where
    toHType x = Defined "PubMedPubDate" [] []
instance XmlContent PubMedPubDate where
    toContents (PubMedPubDate as a b c d) =
        [CElem (Elem (N "PubMedPubDate") (toAttrs as) (toContents a ++
                                                       toContents b ++ toContents c ++
                                                       maybe [] toContents d)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["PubMedPubDate"]
        ; interior e $ return (PubMedPubDate (fromAttrs as))
                       `apply` parseContents `apply` parseContents `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <PubMedPubDate>, "++)
instance XmlAttributes PubMedPubDate_Attrs where
    fromAttrs as =
        PubMedPubDate_Attrs
          { pubMedPubDatePubStatus = definiteA fromAttrToTyp "PubMedPubDate" "PubStatus" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "PubStatus" (pubMedPubDatePubStatus v)
        ]

instance XmlAttrType PubMedPubDate_PubStatus where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "received" = Just PubMedPubDate_PubStatus_received
            translate "accepted" = Just PubMedPubDate_PubStatus_accepted
            translate "epublish" = Just PubMedPubDate_PubStatus_epublish
            translate "ppublish" = Just PubMedPubDate_PubStatus_ppublish
            translate "revised" = Just PubMedPubDate_PubStatus_revised
            translate "aheadofprint" = Just PubMedPubDate_PubStatus_aheadofprint
            translate "retracted" = Just PubMedPubDate_PubStatus_retracted
            translate "pmc" = Just PubMedPubDate_PubStatus_pmc
            translate "pmcr" = Just PubMedPubDate_PubStatus_pmcr
            translate "pubmed" = Just PubMedPubDate_PubStatus_pubmed
            translate "pubmedr" = Just PubMedPubDate_PubStatus_pubmedr
            translate "premedline" = Just PubMedPubDate_PubStatus_premedline
            translate "medline" = Just PubMedPubDate_PubStatus_medline
            translate "medliner" = Just PubMedPubDate_PubStatus_medliner
            translate "entrez" = Just PubMedPubDate_PubStatus_entrez
            translate "pmc-release" = Just PubMedPubDate_PubStatus_pmc_release
            translate _ = Nothing
    toAttrFrTyp n PubMedPubDate_PubStatus_received = Just (N n, str2attr "received")
    toAttrFrTyp n PubMedPubDate_PubStatus_accepted = Just (N n, str2attr "accepted")
    toAttrFrTyp n PubMedPubDate_PubStatus_epublish = Just (N n, str2attr "epublish")
    toAttrFrTyp n PubMedPubDate_PubStatus_ppublish = Just (N n, str2attr "ppublish")
    toAttrFrTyp n PubMedPubDate_PubStatus_revised = Just (N n, str2attr "revised")
    toAttrFrTyp n PubMedPubDate_PubStatus_aheadofprint = Just (N n, str2attr "aheadofprint")
    toAttrFrTyp n PubMedPubDate_PubStatus_retracted = Just (N n, str2attr "retracted")
    toAttrFrTyp n PubMedPubDate_PubStatus_pmc = Just (N n, str2attr "pmc")
    toAttrFrTyp n PubMedPubDate_PubStatus_pmcr = Just (N n, str2attr "pmcr")
    toAttrFrTyp n PubMedPubDate_PubStatus_pubmed = Just (N n, str2attr "pubmed")
    toAttrFrTyp n PubMedPubDate_PubStatus_pubmedr = Just (N n, str2attr "pubmedr")
    toAttrFrTyp n PubMedPubDate_PubStatus_premedline = Just (N n, str2attr "premedline")
    toAttrFrTyp n PubMedPubDate_PubStatus_medline = Just (N n, str2attr "medline")
    toAttrFrTyp n PubMedPubDate_PubStatus_medliner = Just (N n, str2attr "medliner")
    toAttrFrTyp n PubMedPubDate_PubStatus_entrez = Just (N n, str2attr "entrez")
    toAttrFrTyp n PubMedPubDate_PubStatus_pmc_release = Just (N n, str2attr "pmc-release")

instance HTypeable PublicationStatus where
    toHType x = Defined "PublicationStatus" [] []
instance XmlContent PublicationStatus where
    toContents (PublicationStatus a) =
        [CElem (Elem (N "PublicationStatus") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["PublicationStatus"]
        ; interior e $ return (PublicationStatus)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <PublicationStatus>, "++)

instance HTypeable ArticleIdList where
    toHType x = Defined "ArticleIdList" [] []
instance XmlContent ArticleIdList where
    toContents (ArticleIdList a) =
        [CElem (Elem (N "ArticleIdList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ArticleIdList"]
        ; interior e $ return (ArticleIdList) `apply` parseContents
        } `adjustErr` ("in <ArticleIdList>, "++)

instance HTypeable ArticleId where
    toHType x = Defined "ArticleId" [] []
instance XmlContent ArticleId where
    toContents (ArticleId as a) =
        [CElem (Elem (N "ArticleId") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["ArticleId"]
        ; interior e $ return (ArticleId (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <ArticleId>, "++)
instance XmlAttributes ArticleId_Attrs where
    fromAttrs as =
        ArticleId_Attrs
          { articleIdIdType = defaultA fromAttrToTyp ArticleId_IdType_pubmed "IdType" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrTyp "IdType" (articleIdIdType v)
        ]

instance XmlAttrType ArticleId_IdType where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "doi" = Just ArticleId_IdType_doi
            translate "pii" = Just ArticleId_IdType_pii
            translate "pmcpid" = Just ArticleId_IdType_pmcpid
            translate "pmpid" = Just ArticleId_IdType_pmpid
            translate "pmc" = Just ArticleId_IdType_pmc
            translate "mid" = Just ArticleId_IdType_mid
            translate "sici" = Just ArticleId_IdType_sici
            translate "pubmed" = Just ArticleId_IdType_pubmed
            translate "medline" = Just ArticleId_IdType_medline
            translate "pmcid" = Just ArticleId_IdType_pmcid
            translate _ = Nothing
    toAttrFrTyp n ArticleId_IdType_doi = Just (N n, str2attr "doi")
    toAttrFrTyp n ArticleId_IdType_pii = Just (N n, str2attr "pii")
    toAttrFrTyp n ArticleId_IdType_pmcpid = Just (N n, str2attr "pmcpid")
    toAttrFrTyp n ArticleId_IdType_pmpid = Just (N n, str2attr "pmpid")
    toAttrFrTyp n ArticleId_IdType_pmc = Just (N n, str2attr "pmc")
    toAttrFrTyp n ArticleId_IdType_mid = Just (N n, str2attr "mid")
    toAttrFrTyp n ArticleId_IdType_sici = Just (N n, str2attr "sici")
    toAttrFrTyp n ArticleId_IdType_pubmed = Just (N n, str2attr "pubmed")
    toAttrFrTyp n ArticleId_IdType_medline = Just (N n, str2attr "medline")
    toAttrFrTyp n ArticleId_IdType_pmcid = Just (N n, str2attr "pmcid")

instance HTypeable History where
    toHType x = Defined "History" [] []
instance XmlContent History where
    toContents (History a) =
        [CElem (Elem (N "History") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["History"]
        ; interior e $ return (History) `apply` parseContents
        } `adjustErr` ("in <History>, "++)

instance HTypeable URL where
    toHType x = Defined "URL" [] []
instance XmlContent URL where
    toContents (URL as a) =
        [CElem (Elem (N "URL") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["URL"]
        ; interior e $ return (URL (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <URL>, "++)
instance XmlAttributes URL_Attrs where
    fromAttrs as =
        URL_Attrs
          { uRLLang = possibleA fromAttrToTyp "lang" as
          , uRLType = possibleA fromAttrToTyp "Type" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrTyp "lang" (uRLLang v)
        , maybeToAttr toAttrFrTyp "Type" (uRLType v)
        ]

instance XmlAttrType URL_lang where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "AF" = Just URL_lang_AF
            translate "AR" = Just URL_lang_AR
            translate "AZ" = Just URL_lang_AZ
            translate "BG" = Just URL_lang_BG
            translate "CS" = Just URL_lang_CS
            translate "DA" = Just URL_lang_DA
            translate "DE" = Just URL_lang_DE
            translate "EN" = Just URL_lang_EN
            translate "EL" = Just URL_lang_EL
            translate "ES" = Just URL_lang_ES
            translate "FA" = Just URL_lang_FA
            translate "FI" = Just URL_lang_FI
            translate "FR" = Just URL_lang_FR
            translate "HE" = Just URL_lang_HE
            translate "HU" = Just URL_lang_HU
            translate "HY" = Just URL_lang_HY
            translate "IN" = Just URL_lang_IN
            translate "IS" = Just URL_lang_IS
            translate "IT" = Just URL_lang_IT
            translate "IW" = Just URL_lang_IW
            translate "JA" = Just URL_lang_JA
            translate "KA" = Just URL_lang_KA
            translate "KO" = Just URL_lang_KO
            translate "LT" = Just URL_lang_LT
            translate "MK" = Just URL_lang_MK
            translate "ML" = Just URL_lang_ML
            translate "NL" = Just URL_lang_NL
            translate "NO" = Just URL_lang_NO
            translate "PL" = Just URL_lang_PL
            translate "PT" = Just URL_lang_PT
            translate "PS" = Just URL_lang_PS
            translate "RO" = Just URL_lang_RO
            translate "RU" = Just URL_lang_RU
            translate "SL" = Just URL_lang_SL
            translate "SK" = Just URL_lang_SK
            translate "SQ" = Just URL_lang_SQ
            translate "SR" = Just URL_lang_SR
            translate "SV" = Just URL_lang_SV
            translate "SW" = Just URL_lang_SW
            translate "TH" = Just URL_lang_TH
            translate "TR" = Just URL_lang_TR
            translate "UK" = Just URL_lang_UK
            translate "VI" = Just URL_lang_VI
            translate "ZH" = Just URL_lang_ZH
            translate _ = Nothing
    toAttrFrTyp n URL_lang_AF = Just (N n, str2attr "AF")
    toAttrFrTyp n URL_lang_AR = Just (N n, str2attr "AR")
    toAttrFrTyp n URL_lang_AZ = Just (N n, str2attr "AZ")
    toAttrFrTyp n URL_lang_BG = Just (N n, str2attr "BG")
    toAttrFrTyp n URL_lang_CS = Just (N n, str2attr "CS")
    toAttrFrTyp n URL_lang_DA = Just (N n, str2attr "DA")
    toAttrFrTyp n URL_lang_DE = Just (N n, str2attr "DE")
    toAttrFrTyp n URL_lang_EN = Just (N n, str2attr "EN")
    toAttrFrTyp n URL_lang_EL = Just (N n, str2attr "EL")
    toAttrFrTyp n URL_lang_ES = Just (N n, str2attr "ES")
    toAttrFrTyp n URL_lang_FA = Just (N n, str2attr "FA")
    toAttrFrTyp n URL_lang_FI = Just (N n, str2attr "FI")
    toAttrFrTyp n URL_lang_FR = Just (N n, str2attr "FR")
    toAttrFrTyp n URL_lang_HE = Just (N n, str2attr "HE")
    toAttrFrTyp n URL_lang_HU = Just (N n, str2attr "HU")
    toAttrFrTyp n URL_lang_HY = Just (N n, str2attr "HY")
    toAttrFrTyp n URL_lang_IN = Just (N n, str2attr "IN")
    toAttrFrTyp n URL_lang_IS = Just (N n, str2attr "IS")
    toAttrFrTyp n URL_lang_IT = Just (N n, str2attr "IT")
    toAttrFrTyp n URL_lang_IW = Just (N n, str2attr "IW")
    toAttrFrTyp n URL_lang_JA = Just (N n, str2attr "JA")
    toAttrFrTyp n URL_lang_KA = Just (N n, str2attr "KA")
    toAttrFrTyp n URL_lang_KO = Just (N n, str2attr "KO")
    toAttrFrTyp n URL_lang_LT = Just (N n, str2attr "LT")
    toAttrFrTyp n URL_lang_MK = Just (N n, str2attr "MK")
    toAttrFrTyp n URL_lang_ML = Just (N n, str2attr "ML")
    toAttrFrTyp n URL_lang_NL = Just (N n, str2attr "NL")
    toAttrFrTyp n URL_lang_NO = Just (N n, str2attr "NO")
    toAttrFrTyp n URL_lang_PL = Just (N n, str2attr "PL")
    toAttrFrTyp n URL_lang_PT = Just (N n, str2attr "PT")
    toAttrFrTyp n URL_lang_PS = Just (N n, str2attr "PS")
    toAttrFrTyp n URL_lang_RO = Just (N n, str2attr "RO")
    toAttrFrTyp n URL_lang_RU = Just (N n, str2attr "RU")
    toAttrFrTyp n URL_lang_SL = Just (N n, str2attr "SL")
    toAttrFrTyp n URL_lang_SK = Just (N n, str2attr "SK")
    toAttrFrTyp n URL_lang_SQ = Just (N n, str2attr "SQ")
    toAttrFrTyp n URL_lang_SR = Just (N n, str2attr "SR")
    toAttrFrTyp n URL_lang_SV = Just (N n, str2attr "SV")
    toAttrFrTyp n URL_lang_SW = Just (N n, str2attr "SW")
    toAttrFrTyp n URL_lang_TH = Just (N n, str2attr "TH")
    toAttrFrTyp n URL_lang_TR = Just (N n, str2attr "TR")
    toAttrFrTyp n URL_lang_UK = Just (N n, str2attr "UK")
    toAttrFrTyp n URL_lang_VI = Just (N n, str2attr "VI")
    toAttrFrTyp n URL_lang_ZH = Just (N n, str2attr "ZH")

instance XmlAttrType URL_Type where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "FullText" = Just URL_Type_FullText
            translate "Summary" = Just URL_Type_Summary
            translate "fulltext" = Just URL_Type_fulltext
            translate "summary" = Just URL_Type_summary
            translate _ = Nothing
    toAttrFrTyp n URL_Type_FullText = Just (N n, str2attr "FullText")
    toAttrFrTyp n URL_Type_Summary = Just (N n, str2attr "Summary")
    toAttrFrTyp n URL_Type_fulltext = Just (N n, str2attr "fulltext")
    toAttrFrTyp n URL_Type_summary = Just (N n, str2attr "summary")

instance HTypeable ObjectList where
    toHType x = Defined "ObjectList" [] []
instance XmlContent ObjectList where
    toContents (ObjectList a) =
        [CElem (Elem (N "ObjectList") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["ObjectList"]
        ; interior e $ return (ObjectList) `apply` parseContents
        } `adjustErr` ("in <ObjectList>, "++)

instance HTypeable Object where
    toHType x = Defined "Object" [] []
instance XmlContent Object where
    toContents (Object as a) =
        [CElem (Elem (N "Object") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Object"]
        ; interior e $ return (Object (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <Object>, "++)
instance XmlAttributes Object_Attrs where
    fromAttrs as =
        Object_Attrs
          { objectType = definiteA fromAttrToStr "Object" "Type" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "Type" (objectType v)
        ]

instance HTypeable Param where
    toHType x = Defined "Param" [] []
instance XmlContent Param where
    toContents (Param as a) =
        [CElem (Elem (N "Param") (toAttrs as) (toText a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["Param"]
        ; interior e $ return (Param (fromAttrs as))
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <Param>, "++)
instance XmlAttributes Param_Attrs where
    fromAttrs as =
        Param_Attrs
          { paramName = definiteA fromAttrToStr "Param" "Name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "Name" (paramName v)
        ]

instance HTypeable Hour where
    toHType x = Defined "Hour" [] []
instance XmlContent Hour where
    toContents (Hour a) =
        [CElem (Elem (N "Hour") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Hour"]
        ; interior e $ return (Hour) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Hour>, "++)

instance HTypeable Minute where
    toHType x = Defined "Minute" [] []
instance XmlContent Minute where
    toContents (Minute a) =
        [CElem (Elem (N "Minute") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Minute"]
        ; interior e $ return (Minute) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Minute>, "++)

instance HTypeable Second where
    toHType x = Defined "Second" [] []
instance XmlContent Second where
    toContents (Second a) =
        [CElem (Elem (N "Second") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Second"]
        ; interior e $ return (Second) `apply` (text `onFail` return "")
        } `adjustErr` ("in <Second>, "++)



{-Done-}
