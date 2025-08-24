<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name = "nav" match="/">

<nav class="navbar navbar-default navbar-fixed-top">
      <div class="container">
        <div class="navbar-header">
          <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
          <a class="navbar-brand" href="#">Project name</a>
        </div>
        <div id="navbar" class="collapse navbar-collapse">
          <ul class="nav navbar-nav">
            <li class="active"><a href="#">Home</a></li>
            <li><a href="#temporal">Temporal coverage</a></li>
            <li><a href="#geographic">Geographic coverage</a></li>
            <li><a href="#datatable">Data tables</a></li>
		<li><a href="#spatraster">Spatial Rasters</a></li>
		<li><a href="#spatvector">Spatial Vectors</a></li>
            <li><a href="#otherentity">Custom units</a></li>
            <li><a href="#otherentity">Other entities</a></li>
          </ul>
        </div><!--/.nav-collapse -->
      </div>
    </nav>
    
    
</xsl:template>
</xsl:stylesheet>
