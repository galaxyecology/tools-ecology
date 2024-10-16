<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="head.xsl"/> 
<xsl:include href="temporal_coverage.xsl"/> 
<xsl:include href="geographic_coverage.xsl"/>
<xsl:include href="taxonomic_coverage.xsl"/>
<xsl:include href="dataset2.xsl"/>
<xsl:include href="datatable2.xsl"/>
<xsl:include href="spatraster2.xsl"/>
<xsl:include href="spatvector2.xsl"/>
<xsl:include href="units.xsl"/>
<xsl:include href="otherentity.xsl"/>
<xsl:include href="footer.xsl"/>
<xsl:include href="rights.xsl"/>
<xsl:include href="addmap2.xsl"/>
<xsl:include href="method.xsl"/> 
<xsl:template match="/">
<html lang="en">
  
  <xsl:call-template name="head"/>
  <body>
    <div class="container" id="dataset_info">
      <div class="starter-template">
        <xsl:call-template name="dataset2"/>
      </div>
    </div>
    <div class="container" id="method">
        <xsl:call-template name="method"/> 
    </div>

<div class="container" id="temporal">
      <xsl:call-template name="temporal_coverage"/>
    </div><!-- /.container -->
    
    <div class="container" id="geographic">
      <xsl:call-template name="geographic_coverage"/>
      <xsl:call-template name="addmap2"/>
    </div><!-- /.container -->

    <div class="container" id="taxonomic">
      <xsl:call-template name="taxonomic_coverage"/>
    </div><!-- /.container -->
    
    <div class="container" id="datatable">
      <xsl:call-template name="datatable2"/>
    </div><!-- /.container -->
    
    <div class="container" id="spatraster">
      <xsl:call-template name="spatraster2"/>
    </div><!-- /.container -->
    
    <div class="container" id="spavector">
      <xsl:call-template name="spatvector2"/>
    </div><!-- /.container -->
    
    <div class="container" id="units">
      <xsl:call-template name="units"/>
    </div><!-- /.container -->
 
     <div class="container" id="otherentity">
      <xsl:call-template name="otherentity"/>
     </div><!-- /.container -->
    
    <div class="container">
      <xsl:call-template name="rights"/>
    </div>
 
  </body>
 
  <xsl:call-template name="footer"/>
  
</html>

</xsl:template>
</xsl:stylesheet>
