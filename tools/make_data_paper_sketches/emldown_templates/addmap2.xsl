<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template name="addmap2" match="/">
<xsl:choose>
  <xsl:when test=".//geographicCoverage">
      <div class="embed-responsive embed-responsive-4by3">
      <img src="map.png" style="padding:50px" ></img>
      </div>
  </xsl:when>
</xsl:choose>
</xsl:template>
</xsl:stylesheet>
  
