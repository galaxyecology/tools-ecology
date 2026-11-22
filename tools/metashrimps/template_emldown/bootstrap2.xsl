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
<xsl:template match="/">
<html lang="en">
  
  <xsl:call-template name="head"/>
  <body>
    <div class="container" id="dataset_info">
      <div class="starter-template">
        <xsl:call-template name="dataset2"/>
      </div>
    </div>

<div class="container" id="temporal">
      <h3>
<span class="glyphicon glyphicon-time"></span>
 Temporal coverage</h3>
      <xsl:call-template name="temporal_coverage"/>
    </div><!-- /.container -->
    
    <div class="container" id="geographic">
      
      <h3>
        <span class="glyphicon glyphicon-globe"></span>
         Geographic coverage
      </h3>
     
      <xsl:call-template name="addmap2"/>


      <xsl:call-template name="geographic_coverage"/>

    </div><!-- /.container -->

    <div class="container" id="taxonomic">
      <h3>
<span class="glyphicon glyphicon-tree-deciduous"></span>
Taxonomic coverage</h3>
      <xsl:call-template name="taxonomic_coverage"/>
  </div><!-- /.container -->
     <div class="container" id="datatable">
         <h3>   
<span class="glyphicon glyphicon-list-alt"></span>
 Data tables</h3>
      <xsl:call-template name="datatable2"/>
    </div><!-- /.container -->
    
    <div class="container" id="spatraster">
         <h3>   
<span class="glyphicon glyphicon-list-alt"></span>
 Spatial Rasters</h3>
      <xsl:call-template name="spatraster2"/>
    </div><!-- /.container -->
    
    <div class="container" id="spavector">
         <h3>   
<span class="glyphicon glyphicon-list-alt"></span>
 Spatial Vectors</h3>
      <xsl:call-template name="spatvector2"/>
    </div><!-- /.container -->
    
    <div class="container" id="units">
     <h3>
<span class="glyphicon glyphicon-glass"></span>
 Custom units</h3>
      <xsl:call-template name="units"/>
    </div><!-- /.container -->
 
     <div class="container" id="otherentity">
       <h3>
         <span class="glyphicon glyphicon-list-alt"></span>
          Other entities
       </h3>
      <xsl:call-template name="otherentity"/>
    </div><!-- /.container -->
    
    <div class="container">
      <xsl:call-template name="rights"/>
    </div>
 
    <!-- Bootstrap core JavaScript
    ================================================== -->
    <!-- Placed at the end of the document so the pages load faster -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
  </body>

  
  <xsl:call-template name="footer"/>
  
</html>

</xsl:template>
</xsl:stylesheet>
