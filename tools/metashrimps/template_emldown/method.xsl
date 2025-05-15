<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="method" match="/">
  <h4>Method</h4>
<div class="container">
  <p class="editor"><xsl:value-of select="//dataset/methods/methodStep" /></p>
  <p class="editor"><xsl:value-of select="//dataset/methods/sampling" /></p>
</div>


</xsl:template>
</xsl:stylesheet>