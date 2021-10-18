<?xml version="1.0" encoding="UTF-8"?> <xsl:stylesheet version="1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:eml="eml://ecoinformatics.org/eml-2.1.1" 
    xmlns:stmml="http://www.xml-cml.org/schema/stmml" 
    xmlns:sw="eml://ecoinformatics.org/software-2.1.1" 
    xmlns:cit="eml://ecoinformatics.org/literature-2.1.1" 
    xmlns:ds="eml://ecoinformatics.org/dataset-2.1.1" 
    xmlns:prot="eml://ecoinformatics.org/protocol-2.1.1" 
    xmlns:doc="eml://ecoinformatics.org/documentation-2.1.1" 
    xmlns:res="eml://ecoinformatics.org/resource-2.1.1" 
    xmlns:gmd="http://www.isotc211.org/2005/gmd" 
    xmlns:gco="http://www.isotc211.org/2005/gco"
    xmlns:gml="http://www.opengis.net/gml/3.2"
    xmlns:gmx="http://www.isotc211.org/2005/gmx"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" >

<!--xsl:import href="iso2eml-party.xsl"/>
<xsl:import href="iso2eml-coverage.xsl"/>
<xsl:import href="iso2eml-project.xsl"/-->

<xsl:output method="xml" encoding="UTF-8" indent="yes" />
<xsl:strip-space elements="*" />

<xsl:template match="/gmd:MD_Metadata">
<eml:eml>
    <xsl:attribute name="xsi:schemaLocation">eml://ecoinformatics.org/eml-2.1.1 ~/development/eml/eml.xsd</xsl:attribute>
    <!-- Add the packageId -->
    <xsl:attribute name="packageId"><xsl:value-of select="normalize-space(gmd:fileIdentifier/gco:CharacterString)"/></xsl:attribute>
    <xsl:attribute name="system"><xsl:value-of select="'knb'"/></xsl:attribute>
    <xsl:attribute name="scope"><xsl:value-of select="'system'"/></xsl:attribute>
    <dataset>
        <!-- Add the title -->
        <xsl:for-each select="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:title/gco:CharacterString">
            <title><xsl:value-of select="normalize-space(.)"/></title>
        </xsl:for-each>
        
        <!-- Add creators -->
        <xsl:call-template name="creators">
            <xsl:with-param name="doc" select="." />
        </xsl:call-template>

        <!-- Add additional parties -->
        <xsl:call-template name="additional-parties">
            <xsl:with-param name="doc" select="." />
        </xsl:call-template>

        <!-- Add the pubDate if available -->
        <xsl:if test="gmd:dateStamp/gco:DateTime != ''">
            <pubDate>
                <xsl:choose>
                    <xsl:when test="contains(gmd:dateStamp/gco:DateTime, 'T')">
                        <xsl:value-of select="normalize-space(substring-before(gmd:dateStamp/gco:DateTime, 'T'))" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="normalize-space(gmd:dateStamp/gco:DateTime)" />
                    </xsl:otherwise>
                </xsl:choose>
            </pubDate>
        </xsl:if>
            
        <!-- Add the language -->
        <xsl:if test="gmd:language/gco:CharacterString != ''">
            <language><xsl:value-of select="normalize-space(gmd:language/gco:CharacterString)" /></language>           
        </xsl:if>
        
        <!-- Add the abstract -->
        <abstract>
            <para><xsl:value-of select="normalize-space(gmd:identificationInfo/gmd:MD_DataIdentification/gmd:abstract/gco:CharacterString)" /></para>
        </abstract>
        
        <!-- Add keywords -->
        <xsl:if test="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:descriptiveKeywords != ''">
            <xsl:call-template name="keywords">
                <xsl:with-param name="keys" select="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:descriptiveKeywords" />
            </xsl:call-template>
        </xsl:if>

        <!-- Add any gmd:topicCategory fields as keywords too -->
        <xsl:if test="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:topicCategory != ''">
            <xsl:call-template name="topics">
                <xsl:with-param name="topics" select="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:topicCategory" />
            </xsl:call-template>
        </xsl:if>

        <!-- Add intellectual rights -->
        <!-- 
            Note these rules are specific to the arcticdata.io content, 
            and will need to be generalized
        -->
        <xsl:choose>
            <xsl:when test="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:resourceConstraints/gmd:MD_Constraints/gmd:useLimitation">
                <!-- Transfer MD_Constraints/useLimitation directly -->
                <intellectualRights>
                    <para>
                        <xsl:value-of select="normalize-space(gmd:identificationInfo/gmd:MD_DataIdentification/gmd:resourceConstraints/gmd:MD_Constraints/gmd:useLimitation/gco:CharacterString)" />
                    </para>
                </intellectualRights>
            </xsl:when>
            <xsl:otherwise>
                
                <!-- Assign a CC-BY license -->
                <intellectualRights>
                    <para>
                        <xsl:text>This work is licensed under the Creative Commons Attribution 4.0 International License.To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.</xsl:text>
                    </para>
                </intellectualRights>
                
            </xsl:otherwise>
        </xsl:choose>

        <!-- Add distribution -->

        <!-- Add coverage -->
        <xsl:call-template name="coverage" />
            
        <!-- Add contacts -->
        <xsl:call-template name="contacts">
            <xsl:with-param name="doc" select="." />
        </xsl:call-template>

        
        <!-- Add the publisher -->
        <xsl:call-template name="publishers">
            <xsl:with-param name="doc" select="." />
        </xsl:call-template>

        
        <!-- Add the pubPlace  -->
        
        <!-- Add the methods   -->
        
        <!-- Add the project   -->
        <xsl:call-template name="project">
            <xsl:with-param name="doc" select="." />
        </xsl:call-template>
        
        <!-- Add entities      -->
        
    </dataset>
</eml:eml>
</xsl:template>

<!-- Process Keywords and associated thesuarus entries -->
<xsl:template name="keywords">
    <xsl:param name = "keys" />
    <xsl:for-each select="$keys">
        <xsl:variable name="kw-type" select="./gmd:MD_Keywords/gmd:type/gmd:MD_KeywordTypeCode/@codeListValue" />
        <keywordSet>    
            <xsl:for-each select="./gmd:MD_Keywords/gmd:keyword/gco:CharacterString">
                <keyword>
                    <!-- ISO: discipline, place, stratum, temporal, theme -->
                    <!-- EML:             place, stratum, temporal, theme, taxonomic -->
                    <xsl:if test="$kw-type != '' and (
                        $kw-type = 'place' or $kw-type = 'stratum' or 
                        $kw-type = 'temporal' or $kw-type = 'theme')">
                        <xsl:attribute name="keywordType"><xsl:value-of select="normalize-space($kw-type)"/></xsl:attribute>
                    </xsl:if>
                    <xsl:value-of select="normalize-space(.)" />
                </keyword>
            </xsl:for-each>
            <xsl:if test="./gmd:MD_Keywords/gmd:thesaurusName != ''">
                <xsl:choose>
                    <xsl:when test="./gmd:MD_Keywords/gmd:thesaurusName/gmd:CI_Citation/gmd:collectiveTitle != ''">
                        <keywordThesaurus>
                            <xsl:value-of select="normalize-space(./gmd:MD_Keywords/gmd:thesaurusName/gmd:CI_Citation/gmd:collectiveTitle/gco:CharacterString)" />
                        </keywordThesaurus>
                    </xsl:when>
                    <xsl:otherwise>
                        <keywordThesaurus>
                            <xsl:value-of select="normalize-space(./gmd:MD_Keywords/gmd:thesaurusName/gmd:CI_Citation/gmd:title/gco:CharacterString)" />
                        </keywordThesaurus>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:if>
        </keywordSet>
    </xsl:for-each>
</xsl:template>

<!-- Process Topics -->
<xsl:template name="topics">
    <xsl:param name = "topics" />
    <xsl:for-each select="$topics">
        <keywordSet>    
            <xsl:for-each select="./gmd:MD_TopicCategoryCode">
                <keyword>
                    <xsl:value-of select="normalize-space(.)" />
                </keyword>
            </xsl:for-each>
            <keywordThesaurus>ISO 19115:2003 MD_TopicCategoryCode</keywordThesaurus>
        </keywordSet>
    </xsl:for-each>
</xsl:template>

<!-- Match any geographic or temporal coverage elements -->
<xsl:template name="coverage" match="gmd:identificationInfo/gmd:MD_DataIdentification/gmd:extent">
	<!-- Add EML geographic and temporal coverages, if available -->
		<!-- Add geographic coverages -->
        <xsl:variable name="bboxCount" select="count(.//gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicBoundingBox)" />
        <xsl:variable name="temporalCount" select="count(.//gmd:EX_Extent/gmd:temporalElement)" />
        <xsl:variable name="descriptionCount" select="count(.//gmd:EX_Extent/gmd:description)" />
        <xsl:variable name="exDescCount" select="count(.//gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicDescription)" />
        <xsl:if test="$temporalCount + $bboxCount &gt; 0">
    		<coverage>
                <xsl:choose>
                    <xsl:when test="($descriptionCount + $exDescCount) &gt;= $bboxCount">

                        <xsl:variable name="descriptions" >
                            <xsl:if test="//gmd:EX_Extent/gmd:description">
                                <xsl:for-each select=".//gmd:EX_Extent">
                                    <xsl:copy-of select="gmd:description" />
                                    <xsl:value-of select="'. '" />                                                           
                                </xsl:for-each>                            
                            </xsl:if>
                        </xsl:variable>

                        <xsl:variable name="codeDescriptions" >
                            <xsl:for-each select=".//gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicDescription">
                                <xsl:value-of select="gmd:geographicIdentifier/gmd:MD_Identifier/gmd:code/gco:CharacterString" />
                                    <xsl:if test="count(//gmd:EX_GeographicDescription) &gt; 1">
                                        <xsl:value-of select="', '" />
                                    </xsl:if>
                            </xsl:for-each>
                        </xsl:variable>
                        			
                        <xsl:apply-templates select=".//gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicBoundingBox">
            			    <xsl:with-param name="allDescriptions" select="concat($descriptions, $codeDescriptions)" />
            			</xsl:apply-templates> 
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:comment>No geographic description provided</xsl:comment>
                        <xsl:apply-templates select=".//gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicBoundingBox">
            			    <xsl:with-param name="allDescriptions" select="'No geographic description provided.'" />
            			</xsl:apply-templates> 
                    </xsl:otherwise>                
                </xsl:choose>
		
    			<!-- Add temporal coverages -->
    			<xsl:apply-templates select=".//gmd:EX_Extent/gmd:temporalElement" />                
    		</coverage>
        </xsl:if>
		
</xsl:template>
		
<!-- Handle geographic bounding boxes -->
<xsl:template match="gmd:EX_Extent/gmd:geographicElement/gmd:EX_GeographicBoundingBox">
    <xsl:param name="allDescriptions" />
		<xsl:comment>Geographic coverage</xsl:comment>
	<!-- Handle geographic description -->
	<xsl:choose>
		<xsl:when test="$allDescriptions != ''">
			
       		<geographicCoverage>
       			<geographicDescription>
       				<xsl:value-of select="$allDescriptions" />
       			</geographicDescription>
       			<xsl:apply-templates select="../gmd:geographicElement/gmd:EX_GeographicBoundingBox" />
           		<!-- Add bounding coordinates -->
           		<boundingCoordinates>
           			<westBoundingCoordinate>
           				<xsl:value-of select="normalize-space(gmd:westBoundLongitude/gco:Decimal)" />
           			</westBoundingCoordinate>
           			<eastBoundingCoordinate>
           				<xsl:value-of select="normalize-space(gmd:eastBoundLongitude/gco:Decimal)" />
           			</eastBoundingCoordinate>
           			<northBoundingCoordinate>
           				<xsl:value-of select="normalize-space(gmd:northBoundLatitude/gco:Decimal)" />
           			</northBoundingCoordinate>
           			<southBoundingCoordinate>
           				<xsl:value-of select="normalize-space(gmd:southBoundLatitude/gco:Decimal)" />
           			</southBoundingCoordinate>
           		</boundingCoordinates>
       		</geographicCoverage>
			
		</xsl:when>
		<xsl:otherwise>
			
			<!-- Make up a description from the bounding box -->
			<xsl:if test=".//gmd:EX_GeographicBoundingBox">
				
				<geographicCoverage>
					<geographicDescription>
						<xsl:text>This research took place in the area bounded by: </xsl:text>
						<xsl:value-of select="normalize-space(gmd:EX_GeographicBoundingBox/gmd:westBoundLongitude/gco:Decimal)" />
						<xsl:text> West,</xsl:text>
						<xsl:value-of select="normalize-space(gmd:EX_GeographicBoundingBox/gmd:eastBoundLongitude/gco:Decimal)" />
						<xsl:text> East,</xsl:text>
						<xsl:value-of select="normalize-space(gmd:EX_GeographicBoundingBox/gmd:northBoundLatitude/gco:Decimal)" />
						<xsl:text> North,</xsl:text>
						<xsl:value-of select="normalize-space(gmd:EX_GeographicBoundingBox/gmd:southBoundLatitude/gco:Decimal)" />
						<xsl:text> South.</xsl:text>
					</geographicDescription>
               		<!-- Add bounding coordinates -->
               		<boundingCoordinates>
               			<westBoundingCoordinate>
               				<xsl:value-of select="normalize-space(gmd:westBoundLongitude/gco:Decimal)" />
               			</westBoundingCoordinate>
               			<eastBoundingCoordinate>
               				<xsl:value-of select="normalize-space(gmd:eastBoundLongitude/gco:Decimal)" />
               			</eastBoundingCoordinate>
               			<northBoundingCoordinate>
               				<xsl:value-of select="normalize-space(gmd:northBoundLatitude/gco:Decimal)" />
               			</northBoundingCoordinate>
               			<southBoundingCoordinate>
               				<xsl:value-of select="normalize-space(gmd:southBoundLatitude/gco:Decimal)" />
               			</southBoundingCoordinate>
               		</boundingCoordinates>
				</geographicCoverage>
				
			</xsl:if>
		</xsl:otherwise>
	</xsl:choose>		
</xsl:template>

<!-- Handle temporal coverage elements -->
<xsl:template name="temporalCoverage" match="gmd:EX_Extent/gmd:temporalElement">
	<xsl:comment>Temporal coverage</xsl:comment>
	<xsl:choose>
		<xsl:when test="gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod">
			
			<!-- We have a period, use rangeOfDates -->
			<temporalCoverage>
				<rangeOfDates>
					<beginDate>
						<xsl:choose>
							<xsl:when test="contains(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:beginPosition, 'T')">
								<calendarDate>
									<xsl:value-of select="normalize-space(substring-before(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:beginPosition, 'T'))" />
								</calendarDate>
								<time>
									<xsl:value-of select="normalize-space(substring-after(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:beginPosition, 'T'))" />									
								</time>								
							</xsl:when>
							<xsl:otherwise>
								<calendarDate>
									<xsl:value-of select="normalize-space(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:beginPosition)" />
								</calendarDate>								
							</xsl:otherwise>
						</xsl:choose>
					</beginDate>
					<endDate>
						<xsl:choose>
							<xsl:when test="contains(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:endPosition, 'T')">
								<calendarDate>
									<xsl:value-of select="normalize-space(substring-before(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:endPosition, 'T'))" />
								</calendarDate>
								<time>
									<xsl:value-of select="normalize-space(substring-after(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:endPosition, 'T'))" />									
								</time>								
							</xsl:when>
							<xsl:otherwise>
								<calendarDate>
									<xsl:value-of select="normalize-space(gmd:EX_TemporalExtent/gmd:extent/gml:TimePeriod/gml:endPosition)" />
								</calendarDate>								
							</xsl:otherwise>
						</xsl:choose>
					</endDate>
				</rangeOfDates>
			</temporalCoverage>
		</xsl:when>
		<xsl:otherwise>
			
			<!-- No time period, look for time instant -->
			<xsl:if test="gmd:EX_TemporalExtent/gmd:extent/gml:TimeInstant">
				
				<temporalCoverage>
					<singleDateTime>
						<calendarDate>
							<xsl:value-of select="normalize-space(gmd:EX_TemporalExtent/gmd:extent/gml:TimeInstant/gml:timePosition)" />
						</calendarDate>
					</singleDateTime>
				</temporalCoverage>
				
			</xsl:if>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

    <!-- Match any gmd:credit elements, and if they are present, add a project entry with funding fields -->
    <xsl:template name="project">
        <xsl:param name = "doc" />
        <xsl:variable name="awardCount" select="count(./gmd:identificationInfo/gmd:MD_DataIdentification/gmd:credit)" />
        <!-- Add funding elements -->
        <xsl:if test="$awardCount &gt; 0">
            <project>
                <!-- Add the project title -->
                <title><xsl:value-of select="normalize-space(./gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:title[1]/gco:CharacterString)"/></title>

                <!-- Add the project abstract -->
                <xsl:if test='./gmd:identificationInfo/gmd:MD_DataIdentification/gmd:abstract[1]/gco:CharacterString != ""'>
                    <abstract><xsl:value-of select="normalize-space(./gmd:identificationInfo/gmd:MD_DataIdentification/gmd:abstract[1]/gco:CharacterString)"/></abstract>
                </xsl:if>

                <!-- Add personnel from the PI list or the author list -->
                <xsl:choose>
                    <!-- Select PIs from the citation -->
                    <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator" or @codeListValue="coPrincipalInvestigator" or @codeListValue="collaboratingPrincipalInvestigator"]]!=""'>
                        <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator" or @codeListValue="coPrincipalInvestigator" or @codeListValue="collaboratingPrincipalInvestigator"]]'>
                            <personnel>
                                <xsl:call-template name="party">
                                    <xsl:with-param name="party" select = "." />
                                </xsl:call-template>
                                <role>principalInvestigator</role>
                            </personnel>
                        </xsl:for-each>
                    </xsl:when>
                    <!-- Alternatively, select PIs from anywhere in the doc -->
                    <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator" or @codeListValue="coPrincipalInvestigator" or @codeListValue="collaboratingPrincipalInvestigator"]] != ""'>
                        <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator" or @codeListValue="coPrincipalInvestigator" or @codeListValue="collaboratingPrincipalInvestigator"]]'>
                            <personnel>
                                <xsl:call-template name="party">
                                    <xsl:with-param name="party" select = "." />
                                </xsl:call-template>
                                <role>principalInvestigator</role>
                            </personnel>
                        </xsl:for-each>
                    </xsl:when>
                    <!-- Otherwise, select the author anywhere in the document -->
                    <xsl:otherwise>
                        <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]]'>
                            <personnel>
                                <xsl:call-template name="party">
                                    <xsl:with-param name="party" select = "." />
                                </xsl:call-template>
                                <role>principalInvestigator</role>
                            </personnel>
                        </xsl:for-each>
                    </xsl:otherwise>
                </xsl:choose>

                <!-- Add all of the funding from gmd:credit -->
                <funding>
                    <xsl:for-each select="./gmd:identificationInfo/gmd:MD_DataIdentification/gmd:credit">
                        <para><xsl:value-of select="."/></para>
                    </xsl:for-each>
                </funding>
            </project>
        </xsl:if>
            
    </xsl:template>

<!-- Add creator -->
<xsl:template name="creators">
    <xsl:param name = "doc" />
    <xsl:choose>
        <!-- First add any authors from the gmd:citation -->
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]]'>
                <creator>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </creator>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add authors from anywhere in the document -->
        <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]] != "" '>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]]'>
                <creator>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </creator>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add principalInvestigators from anywhere in the document -->
        <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator"]] != "" '>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator"]]'>
                <creator>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </creator>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add pointOfContact from the citation in the document -->
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]] != "" '>
            <xsl:for-each select='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]]'>
                <creator>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </creator>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add pointOfContact from anywhere in the document -->
        <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]] != "" '>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]]'>
                <creator>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </creator>
            </xsl:for-each>
        </xsl:when>
        <!-- Finally, if all else fails, add the Arctic Data Center -->
        <xsl:otherwise>
            <creator>
                <organizationName>NSF Arctic Data Center</organizationName>
            </creator>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- Add associatedParty: principalInvestigator
    First, check to see if principalInvestigators are listed in the gmd:citation, and if so, use them;
    If not, then search the whole document and use any found.  This avoids duplication.
-->
<xsl:template name="additional-parties">
    <xsl:param name = "doc" />
    <!-- Roles to be handled: originator|principalInvestigator|resourceProvider|distributor -->

    <!-- principalInvestigators -->
    <xsl:choose>
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>principalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="principalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>principalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>

    <!-- coPrincipalInvestigator -->
    <xsl:choose>
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="coPrincipalInvestigator"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="coPrincipalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>coPrincipalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="coPrincipalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>coPrincipalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>

    <!-- collaboratingPrincipalInvestigator -->
    <xsl:choose>
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="collaboratingPrincipalInvestigator"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="collaboratingPrincipalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>collaboratingPrincipalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="collaboratingPrincipalInvestigator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>collaboratingPrincipalInvestigator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>

    <!-- originators -->
    <xsl:choose>
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="originator"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="originator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>originator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="originator"]]'>
                <associatedParty>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                    <role>originator</role>
                </associatedParty>
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>


<!-- Add contacts -->
<xsl:template name="contacts">
    <xsl:param name = "doc" />
    <xsl:choose>
        <!-- Add contacts from the citation in the document -->
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointOfContact/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:pointOfContact/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]]'>
                <contact>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </contact>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add contacts from anywhere in the document -->
        <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]] != "" '>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="pointOfContact"]]'>
                <contact>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </contact>
            </xsl:for-each>
        </xsl:when>
        <!-- Alternatively, add the first author as a contact -->
        <!--
        <xsl:when test='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]] != "" '>
            <contact>
                <xsl:call-template name="party">
                    <xsl:with-param name="party" select = '$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="author"]][1]' />
                </xsl:call-template>
            </contact>
        </xsl:when>
        -->
        <!-- Finally, if all else fails, add the Arctic Data Center -->
        <xsl:otherwise>
            <contact>
                <organizationName>NSF Arctic Data Center</organizationName>
            </contact>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- Add publishers
    First, check to see if publishers are listed in the gmd:citation, and if so, use them;
    If not, then search the whole document and use any found.  This avoids duplication.
-->
<xsl:template name="publishers">
    <xsl:param name = "doc" />
    <!-- publisher -->
    <xsl:choose>
        <xsl:when test='$doc/gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="publisher"]]!=""'>
            <xsl:for-each select='gmd:identificationInfo/gmd:MD_DataIdentification/gmd:citation/gmd:CI_Citation/gmd:citedResponsibleParty/gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="publisher"]]'>
                <publisher>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </publisher>
            </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
            <xsl:for-each select='$doc//gmd:CI_ResponsibleParty[gmd:role/gmd:CI_RoleCode[@codeListValue="publisher"]]'>
                <publisher>
                    <xsl:call-template name="party">
                        <xsl:with-param name="party" select = "." />
                    </xsl:call-template>
                </publisher>
            </xsl:for-each>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- Handle eml-party fields -->    
<xsl:template name="party">
    <xsl:param name = "party" />
    <xsl:apply-templates />   
    <xsl:if test="$party//gmd:voice/gco:CharacterString!=''">
        <phone><xsl:value-of select="normalize-space($party//gmd:voice/gco:CharacterString)"/></phone>
    </xsl:if>
    <xsl:if test="$party//gmd:CI_Address/gmd:electronicMailAddress/gco:CharacterString!=''">
        <electronicMailAddress><xsl:value-of select="normalize-space($party//gmd:CI_Address/gmd:electronicMailAddress/gco:CharacterString)"/></electronicMailAddress>
    </xsl:if>
    <xsl:if test="$party//gmd:onlineResource/gmd:CI_OnlineResource/gmd:linkage/gmd:URL!=''">
        <onlineUrl><xsl:value-of select="normalize-space($party//gmd:onlineResource/gmd:CI_OnlineResource/gmd:linkage/gmd:URL)"/></onlineUrl>
    </xsl:if>
</xsl:template>

<!-- Add an individualName -->
<xsl:template match="gmd:individualName">
    <individualName>
        <surName><xsl:value-of select="normalize-space(gco:CharacterString)"/></surName>
    </individualName>
</xsl:template>

<!-- Add an organizationName -->
<xsl:template match="gmd:organisationName">
    <organizationName><xsl:value-of select="normalize-space(gco:CharacterString)"/></organizationName>
</xsl:template>

<!-- Add a positionName -->
<xsl:template match="gmd:positionName">
    <positionName><xsl:value-of select="normalize-space(gco:CharacterString)"/></positionName>
</xsl:template>

<!-- voice, email, and role are all noops so they can be reordered correctly -->
<xsl:template match="gmd:voice" />
<xsl:template match="gmd:electronicMailAddress" />
<xsl:template match="gmd:role" />
<xsl:template match="gmd:onlineResource" />

<!-- Add an Address -->
<xsl:template match="gmd:CI_Address"> 
    <xsl:if test="gmd:deliveryPoint/gco:CharacterString!='' or gmd:city/gco:CharacterString!='' or gmd:administrativeArea/gco:CharacterString!='' or gmd:postalCode/gco:CharacterString!='' or gmd:country/gco:CharacterString!=''">
    <address>
        <xsl:if test="gmd:deliveryPoint/gco:CharacterString!=''">
            <deliveryPoint><xsl:value-of select="normalize-space(gmd:deliveryPoint/gco:CharacterString)"/></deliveryPoint>
        </xsl:if>
        <xsl:if test="gmd:city/gco:CharacterString!=''">
            <city><xsl:value-of select="normalize-space(gmd:city/gco:CharacterString)"/></city>
        </xsl:if>
        <xsl:if test="gmd:administrativeArea/gco:CharacterString!=''">
            <administrativeArea><xsl:value-of select="normalize-space(gmd:administrativeArea/gco:CharacterString)"/></administrativeArea>
        </xsl:if>
        <xsl:if test="gmd:postalCode/gco:CharacterString!=''">
            <postalCode><xsl:value-of select="normalize-space(gmd:postalCode/gco:CharacterString)"/></postalCode>
        </xsl:if>
        <xsl:if test="gmd:country/gco:CharacterString!=''">
            <country><xsl:value-of select="normalize-space(gmd:country/gco:CharacterString)"/></country>
        </xsl:if>
    </address>
    </xsl:if>
</xsl:template>

</xsl:stylesheet>
