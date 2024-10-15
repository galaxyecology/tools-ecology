<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="method" match="/">
  <h3>Method</h3>

<div class="container">
  <xsl:for-each select="//dataset/methods">
      <xsl:choose>
        <xsl:when test=".//methodStep">
           <h4>Method step</h4>
            <p class="editor"><xsl:value-of select="methodStep" /></p>
        </xsl:when>
      </xsl:choose>
      
      <xsl:choose>
        <xsl:when test=".//sampling">
           <h4>Sampling</h4>
           <p class="editor"><xsl:value-of select="sampling" /></p>
        </xsl:when>
      </xsl:choose>
      
      <xsl:choose>
        <xsl:when test=".//qualityControl">
          <h4>Quality control</h4>
          <p class="editor"><xsl:value-of select="qualityControl" /></p>
        </xsl:when>
      </xsl:choose>
      
  </xsl:for-each>  
</div>
</xsl:template>
</xsl:stylesheet>
