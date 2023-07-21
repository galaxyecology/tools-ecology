<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="dataset" match="/">
  <h1 ><xsl:value-of select="//dataset/title" /></h1>
  <h3 class="editor"> <xsl:value-of select="//dataset/pubDate" /></h3>
<h4><a href="#datatable">
  <span class="glyphicon glyphicon-list-alt"></span>
    Data table(s) <span class="badge"><xsl:value-of select="count(//dataTable)" /></span></a></h4>
  <h4><a href="#spatraster">
  <span class="glyphicon glyphicon-list-alt"></span>
    Spatial Raster(s) <span class="badge"><xsl:value-of select="count(//spatialRaster)" /></span></a></h4>
  <h4><a href="#spatvector">
  <span class="glyphicon glyphicon-list-alt"></span>
    Spatial Vector(s) <span class="badge"><xsl:value-of select="count(//spatialVector)" /></span></a></h4>
  <h4><a href="#otherentity">
  <span class="glyphicon glyphicon-list-alt"></span>
    Other entity(ies) <span class="badge"><xsl:value-of select="count(//otherEntity)" /></span></a></h4>
  <h4>Abstract</h4>
<div class="container">
<div class="editor">
<div class="jumbotron">
  <p class="lead"><xsl:value-of select="//dataset/abstract" /></p>
</div>
</div>
</div>

  <h4>Metadata Provider</h4>
  <p>
  <div name="metaprovider" class="editor">
  <xsl:value-of select="//metadataProvider/individualName/givenName"/>&#160;
  <xsl:value-of select="//metadataProvider/individualName/surName"/>
  </div>
  </p>
  
  <div class="container">
  <div class="panel-group">
    <div class="panel panel-default">
      <div class="panel-heading">
        <h4 class="panel-title">
          <a data-toggle="collapse" href="#collapse1">Author list</a>
        </h4>
      </div>
      <div id="collapse1" class="panel-collapse collapse">
        <h4>Creators</h4>
        <ul class="list-group">
        <xsl:for-each select="//dataset/creator">
          <li class="list-group-item">
		  <div class="editor">
            <xsl:value-of select="individualName/givenName"/>&#160;
            <xsl:value-of select="individualName/surName"/>&#160;
            <xsl:value-of select="electronicMailAddress"/>
		</div>
          </li>
        </xsl:for-each>
        </ul>
        <h4>Associated parties</h4>
        <ul class="list-group">
        <xsl:for-each select="//dataset/associatedParty">
		<div class="editor">
          <li class="list-group-item">
            <xsl:value-of select="individualName/givenName"/>&#160;
            <xsl:value-of select="individualName/surName"/>&#160;
            <xsl:value-of select="electronicMailAddress"/>
          </li>
		  </div>
        </xsl:for-each>
        </ul>
      </div>
    </div>
  </div>
  </div>

  <h4>Keywords</h4>
  
  <p>
  <div class="editor">
  <xsl:for-each select="//dataset/keywordSet/keyword">
   <ul>
     <li><xsl:value-of select="."/></li>
   </ul>
  </xsl:for-each>
  </div>
  </p>
  
 <h4>Annotations</h4>
  
  <p>
  <div class="editor">
  <xsl:for-each select="//dataset/annotation">
   <ul>
	<li>
	<a href="{propertyURI}"><xsl:value-of select="./propertyURI/@label"/></a>
	<xsl:text> </xsl:text>
	<a href="{valueURI}"><xsl:value-of select="./valueURI/@label"/></a> 
	</li>
   </ul>
  </xsl:for-each>
  </div>
  </p>

</xsl:template>
</xsl:stylesheet>
