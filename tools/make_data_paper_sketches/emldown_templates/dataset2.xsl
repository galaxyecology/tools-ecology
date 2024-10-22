<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="dataset2" match="/">
<xsl:choose>
  <xsl:when test=".//dataset/pubDate">
  <h3>Publication date </h3>
  <xsl:value-of select="//dataset/pubDate"/>
  </xsl:when>
</xsl:choose>

<xsl:choose>
  <xsl:when test=".//dataset/abstract">
  <h3>Abstract</h3>
<div class="container">
<div class="jumbotron">
  <p class="lead"><xsl:value-of select="//dataset/abstract" /></p>
</div>
</div>
   </xsl:when>
</xsl:choose>

<xsl:choose>
  <xsl:when test=".//metadataProvider">
  <h3>Metadata Provider</h3>
  <p>
  <xsl:value-of select="//metadataProvider/individualName/givenName"/>&#160;
  <xsl:value-of select="//metadataProvider/individualName/surName"/>
  </p>
  </xsl:when>
</xsl:choose>  
        <h3>Author list</h3>
        <h4>Creators</h4>
        <ul class="list-group">
        <xsl:for-each select="//dataset/creator">
          <li class="list-group-item">
            <xsl:value-of select="individualName/givenName"/>&#160;
            <xsl:value-of select="individualName/surName"/>&#160;
            <xsl:value-of select="electronicMailAddress"/>
          </li>
        </xsl:for-each>
        </ul>

<xsl:choose>
  <xsl:when test=".//associatedParty">       
        <h4>Associated parties</h4>
        <ul class="list-group">
        <xsl:for-each select="//dataset/associatedParty">
          <li class="list-group-item">
            <xsl:value-of select="individualName/givenName"/>&#160;
            <xsl:value-of select="individualName/surName"/>&#160;
            <xsl:value-of select="electronicMailAddress"/>
          </li>
        </xsl:for-each>
        </ul>
 </xsl:when>
</xsl:choose> 
<xsl:choose>
  <xsl:when test=".//dataset/keywordSet/keyword">  
  <h3>Keywords</h3>
  <p>
  <xsl:for-each select="//dataset/keywordSet/keyword">
   <ul>
     <li><xsl:value-of select="."/></li>
   </ul>
  </xsl:for-each>
  </p>
  </xsl:when>
</xsl:choose>

<xsl:choose>
  <xsl:when test=".//dataset/annotation">  
 <h3>Annotations</h3> 
  <p>
  <xsl:for-each select="//dataset/annotation">
   <ul>
	<li>
<xsl:value-of select="./valueURI/@label"/>&#160;
<a href="{valueURI}"><xsl:value-of select="./valueURI"/></a> 
	</li>
   </ul>
  </xsl:for-each>
  </p>
  </xsl:when>
</xsl:choose>
</xsl:template>
</xsl:stylesheet>
