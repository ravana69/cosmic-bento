// If you are reading this and you know how to tell
// whether a browser supports THREE.FloatType without
// browser sniffing, please let me know!
const isMobileApple = /(iPad|iPhone).+(like\sMac)/.test(navigator.userAgent);

const fovTmp = new THREE.Vector4();
const posTmp = new THREE.Vector3();
const spherical = new THREE.Spherical(5, Math.PI * 0.5, 0);
const zero = new THREE.Vector3(0, 0, 0);
const hp = Math.PI * 0.5;
const mouseRot = Math.PI * 0.1;
let autoMove = true;

function spaceScene() {
  const sky = new THREE.Group();
  
  const nStars = 5000;
  const points = new Float32Array(nStars * 3);
  const rand = new Float32Array(nStars);
  
  for (let i = 0; i < nStars; i++) {
    const theta = Math.random() * Math.PI * 2.0;
    const phi = Math.acos(Math.random() * 2.0 - 1.0);
    
    spherical.set(100, phi, theta);
    posTmp.setFromSpherical(spherical);
    posTmp.toArray(points, i * 3);

    rand[i] = Math.random();
  }
  
  const starsGeo = new THREE.BufferGeometry();
  starsGeo.setAttribute('position', new THREE.BufferAttribute(points, 3));
  starsGeo.setAttribute('random', new THREE.BufferAttribute(rand, 1));
  const starsMat = new THREE.ShaderMaterial({
    transparent: true,
    blending: THREE.AdditiveBlending,
    depthTest: true,
    depthWrite: false,
    vertexShader: `
    
attribute float random;
    
varying vec3 vPos;

void main() {
  vPos = position;
  vec4 mvPosition = modelViewMatrix * vec4(position, 1.0);
  gl_Position = projectionMatrix * mvPosition;
  gl_PointSize = 10.0 + random * 10.0;
}

`,
    fragmentShader: `

varying vec3 vPos;

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}

void main() {
  vec2 uv = vec2(gl_PointCoord.x, 1.0 - gl_PointCoord.y);
  vec2 centre = uv - 0.5;
  
  vec3 rgb = vec3(
    rand(vPos.xy),
    rand(vPos.xz),
    rand(vPos.yz)
  );

  vec3 col = vec3(0.5) + rgb * 0.5;
  
  float a = smoothstep(0.8, 1.0, 1.0 - length(centre) * 2.0);
  

  gl_FragColor = vec4(col, a);
}

`
  });

  const stars = new THREE.Points(starsGeo, starsMat);
  sky.add(stars);
  
  const planetGeo = new THREE.SphereGeometry(1, 64, 64);
  const planetMat = new THREE.ShaderMaterial({
    uniforms: {
      uTime: { value: 0 }
    },
    transparent: true,
    vertexShader: `

varying vec3 vPos;

void main() {
  vPos = position;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
}

`,
  fragmentShader: `

varying vec3 vPos;
uniform float uTime;

//	Simplex 3D Noise 
//	by Ian McEwan, Ashima Arts
//
vec4 permute(vec4 x){return mod(((x*34.0)+1.0)*x, 289.0);}
vec4 taylorInvSqrt(vec4 r){return 1.79284291400159 - 0.85373472095314 * r;}

float snoise(vec3 v){ 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //  x0 = x0 - 0. + 0.0 * C 
  vec3 x1 = x0 - i1 + 1.0 * C.xxx;
  vec3 x2 = x0 - i2 + 2.0 * C.xxx;
  vec3 x3 = x0 - 1. + 3.0 * C.xxx;

// Permutations
  i = mod(i, 289.0 ); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients
// ( N*N points uniformly over a square, mapped onto an octahedron.)
  float n_ = 1.0/7.0; // N=7
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z *ns.z);  //  mod(p,N*N)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3))); 
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

vec3 hex2rgb(int hex) {
  return vec3(
    float((hex >> 16) & 0xFF) / 255.0,
    float((hex >> 8) & 0xFF) / 255.0,
    float(hex & 0xFF) / 255.0
  );
}

mat2 rotate2d(float _angle) {
  return mat2(
    cos( _angle ), -sin( _angle ),
    sin( _angle ), cos( _angle )
  );
}

// https://www.shadertoy.com/view/fd3SRf
float sdHexagon( vec2 p, float s, float r ) 
{
    const vec3 k = vec3(-0.866025404,0.5,0.577350269);
    p = abs(p);
    p -= 2.0*min(dot(k.xy,p),0.0)*k.xy;
    p -= vec2(clamp(p.x, -k.z*s, k.z*s), s);
    return length(p)*sign(p.y) - r;
}

#define PI 3.1415926
#define HP PI * 0.5

void main() {
  float len = length(vPos);
  vec3 position = vPos;
  position.xz *= rotate2d(uTime * 0.0001);
  vec3 rotated = position;
  position.y = acos(position.y);

  vec3 samplePos = position * vec3(1.0, 50.0, 1.0);
  float noise = snoise(samplePos);
  vec3 color1 = hex2rgb(0xfcdf9b);
  vec3 color2 = hex2rgb(0xd7ba92);
  vec3 color = mix(color1, color2, noise * 0.5 + 0.5);
  
  samplePos = position * vec3(0.2, 2.0, 0.2);
  noise = snoise(samplePos);
  color1 = hex2rgb(0xa28973);
  color2 = hex2rgb(0xd3a875);
  vec3 secondary = mix(color1, color2, noise * 0.5 + 0.5);
  float colorMix = snoise(position * vec3(0.01, 4.0, 0.01));
  color = mix(color, secondary, colorMix * 0.5 + 0.5);
  
  vec3 stormColor1 = hex2rgb(0xbb915f);
  vec3 stormColor2 = hex2rgb(0xcca270);

  float stormPhi = PI * 0.36;
  float stormTheta = PI * -0.125;
  float stormPR = sin(stormPhi) * len;
  vec3 stormPos = vec3(
    stormPR * sin(stormTheta),
    cos(stormPhi) * len,
    stormPR * cos(stormTheta)
  ); 
  
  vec3 stormProd = stormPos * rotated;
  float stormDist = len * acos((stormProd.x + stormProd.y + stormProd.z) / pow(len, 2.0));
  float storm = smoothstep(0.05, 0.15, stormDist);
  
  vec3 stormRot = position * 4.0;
  stormRot.xy *= rotate2d(stormDist * 0.1 * PI * 4.0);
  float stormNoise = snoise(stormRot);
  vec3 stormColor = mix(stormColor1, stormColor2, stormNoise * 0.5 + 0.5);  
  
  color = mix(stormColor, color, storm);
  
  samplePos = position * vec3(1.0, 7.0, 2.0);
  noise = snoise(samplePos);
  color = mix(color, vec3(1.0) * noise, smoothstep(0.5, 1.0, noise) * 0.7);
  
  vec2 polePos = vec2(0.0, -0.5);
  vec2 sPos = vec2(position.y, length(position.xz));
  float poleDist = 1.0 - smoothstep(0.5, 1.0, length(sPos - polePos));
  color = mix(color, vec3(0.95, 0.98, 1.0), poleDist * 0.95);

  vec3 shadowA = vec3(PI * -0.2, PI * 0.2, PI * 0.2);
  float shadowTheta = atan(shadowA.x, shadowA.z);
  float shadowPhi = acos(clamp(shadowA.y / len, -1.0, 1.0));
  
  float sinPhiRadius = sin(shadowPhi) * len;
  vec3 shadowPos = vec3(
    sinPhiRadius * sin(shadowTheta),
    cos(shadowPhi) * len,
    sinPhiRadius * cos(shadowTheta)
  );
  
  vec3 prod = shadowPos * vPos;
  float dist = len * acos((prod.x + prod.y + prod.z) / pow(len, 2.0));
  color = mix(color, color * 0.1, smoothstep(HP - 0.6, HP - 0.2, dist));
  
  gl_FragColor = vec4(color, 1.0);
}

`
  });
  
  const planet = new THREE.Mesh(planetGeo, planetMat);
  sky.add(planet);

  const ringsGeo = new THREE.PlaneGeometry(1, 1);
  const ringsMat = new THREE.ShaderMaterial({
    uniforms: {
      uTime: { value: 0 }
    },
    transparent: true,
    side: THREE.DoubleSide,
    vertexShader: `

varying vec2 vUv;

void main() {
  vUv = uv;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
}

`,
    fragmentShader: `

varying vec2 vUv;

uniform float uTime;

float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
}

// Simplex 2D noise
//
vec3 permute(vec3 x) { return mod(((x*34.0)+1.0)*x, 289.0); }

float snoise(vec2 v){
  const vec4 C = vec4(0.211324865405187, 0.366025403784439,
           -0.577350269189626, 0.024390243902439);
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);
  vec2 i1;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;
  i = mod(i, 289.0);
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
  + i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy),
    dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;
  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;
  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

#define PI 3.1415926

vec3 hex2rgb(int hex) {
  return vec3(
    float((hex >> 16) & 0xFF) / 255.0,
    float((hex >> 8) & 0xFF) / 255.0,
    float(hex & 0xFF) / 255.0
  );
}

void main() {
  vec2 uv = vUv - 0.5;
  float len = length(uv * 2.0);
  float color = mix(0.7, 0.8, snoise(vec2(len * 400.0, 0.7) * 0.5 + 0.5));
  
  float len2 = length(uv * 50.0);
  float bk = smoothstep(-0.775, -0.65, snoise(vec2(len2, 2.65)));
  
  float alpha = smoothstep(0.9, 0.9125, sin(len * 3.1415926));
  alpha *= step(0.3, len) * smoothstep(0.25, 0.7, len);
  alpha *= bk;
  
  float uvA = atan(uv.y, uv.x);
  float uvL = length(uv);
  
  float shadowA = PI * -0.25;
  vec2 shadowPos = vec2(cos(shadowA) * uvL, sin(shadowA) * uvL);
  
  color *= clamp(smoothstep(0.125, 0.13, distance(uv, shadowPos)), 0.1, 1.0);
  
  vec3 color1 = hex2rgb(0xfcdfb9);
  vec3 color2 = hex2rgb(0xf7faf2);
  
  vec3 final = mix(color1, color2, color) * color * 1.5;
  
  gl_FragColor = vec4(final, alpha);
}
  
`
  });
  
  const rings = new THREE.Mesh(ringsGeo, ringsMat);
  rings.scale.setScalar(8);
  rings.rotation.x = hp;
  
  sky.add(rings);
  
  const updateSky = (now) => {
    planetMat.uniforms.uTime.value = now;
    ringsMat.uniforms.uTime.value = now;
  };

  return { sky, updateSky };
}

function createScene(scene, renderer) {
  
  const geo = new THREE.PlaneGeometry(1, 1);
  const bento = new THREE.ShaderMaterial({
    uniforms: {
      uResolution: { value: new THREE.Vector2() },
      uSceneTexture: { value: null }
    },
    vertexShader: `

varying vec2 vUv;

void main() {
  vUv = uv;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
}

`,
    fragmentShader: `

varying vec2 vUv;

uniform vec2 uResolution;
uniform sampler2D uSceneTexture;

// from https://iquilezles.org/articles/distfunctions
float roundedBoxSDF(vec2 CenterPosition, vec2 Size, float Radius) {
    return length(max(abs(CenterPosition)-Size+Radius,0.0))-Radius;
}

#define E 0.0005

void main() {
  vec2 ar = vec2(uResolution.x / uResolution.y, 1.0);
  vec2 uv = vUv * ar;
  vec2 border = vec2(0.02);
  float colWidth = ar.x * 0.5;
  vec2 size = vec2(ar.x * 0.5, 1.0) - border * vec2(1.5, 2.0);

  vec2 box1C = border + size * 0.5;
  vec2 box2C = vec2(ar.x - box1C.x, border.y * 0.75 + size.y * 0.25);
  vec2 box3C = vec2(ar.x - box1C.x, 1.0 - box2C.y);
  
  float d1 = roundedBoxSDF(uv - box1C, size * 0.5, 0.02);
  float d2 = roundedBoxSDF(uv - box2C, size * vec2(0.5, 0.25 - border.y * 0.25), 0.02);
  float d3 = roundedBoxSDF(uv - box3C, size * vec2(0.5, 0.25 - border.y * 0.25), 0.02);
  
  vec3 bg = texture2D(uSceneTexture, vUv).rgb;

  float bentoBox = smoothstep(-E, E, d1)
    * smoothstep(-E, E, d2)
    * smoothstep(-E, E, d3);
  vec3 col = mix(bg, vec3(1), bentoBox);

  gl_FragColor = vec4(col, 1.0);
}
`
  });
  const plane = new THREE.Mesh(geo, bento);
  scene.add(plane);

  const target = new THREE.WebGLRenderTarget(1024, 1024, {
    format: THREE.RGBAFormat,
    type: isMobileApple ? THREE.HalfFloatType : THREE.FloatType
  });
  
  bento.uniforms.uSceneTexture.value = target.texture;
  
  const { sky, updateSky } = spaceScene();
  
  const osScene = new THREE.Scene();
  osScene.add(sky);

  const scenes = [
    {
      scene: osScene,
      bg: 0x020204,
      fov: 75,
      phi: 1,
      theta: Math.PI * 0.25,
      radius: 2,
      viewport: new THREE.Vector4(0, 0, 0.5, 1),
      rotTarget: new THREE.Vector3(0, 0, 0),
      rotation: new THREE.Vector3(0, 0, 0),
      focus: new THREE.Vector3(1, -2.5, -3)
    },
    {
      scene: osScene,
      bg: 0x020204,
      fov: 45,
      phi: Math.PI * 0.35,
      theta: 0,
      radius: 8,
      viewport: new THREE.Vector4(0.5, 0.5, 0.5, 0.5),
      rotTarget: new THREE.Vector3(0, 0, 0),
      rotation: new THREE.Vector3(0, 0, 0),
      focus: new THREE.Vector3(0, 0, 0)
    },
    {
      scene: osScene,
      bg: 0x020204,
      fov: 55,
      phi: Math.PI * 0.1,
      theta: 0,
      radius: 1.5,
      viewport: new THREE.Vector4(0.5, 0, 0.5, 0.5),
      rotTarget: new THREE.Vector3(0, 0, 0),
      rotation: new THREE.Vector3(0, 0, 0),
      focus: new THREE.Vector3(0, -2, -3)
    },
  ];
  
  scenes.forEach((scene) => {
    spherical.set(scene.radius, scene.phi, scene.theta);
    scene.rotTarget.setFromSpherical(spherical);
    scene.rotation.lerp(scene.rotTarget, 0.8);
  });
  
  const offscreenCamera = new THREE.PerspectiveCamera(75, 1, 0.001, 1000);
  offscreenCamera.position.z = 5;
  
  const update = (now) => {
    updateSky(now);
  };
  
  return { bento, target, scenes, offscreenCamera, update };
}

const doRender = (renderer, color, scene, camera, viewport, fov) => {
  renderer.setClearColor(color);

  renderer.setViewport(viewport);
  renderer.setScissor(viewport);

  camera.fov = fov;
  camera.aspect = viewport.z / viewport.w;
  camera.updateProjectionMatrix();

  renderer.render(scene, camera);
};

function updateScene({ renderer, sceneObjects }) {
  const { bento, target, scenes, offscreenCamera, update } = sceneObjects;
  
  const size = bento.uniforms.uResolution.value;
  renderer.getSize(size);
  
  renderer.setRenderTarget(target);
  renderer.setScissorTest(true);
  
  if (autoMove) {
    mouse.x = Math.cos(performance.now() * 0.0001);
    mouse.y = Math.sin(performance.now() * 0.0001);
  }

  for (let i = 0; i < scenes.length; i++) {
    const scene = scenes[i];
    fovTmp.set(size.x, size.y, size.x, size.y);
    fovTmp.multiply(scene.viewport);
  
    spherical.set(
      scene.radius,
      scene.phi + mouseRot * -mouse.y,
      scene.theta + mouseRot * mouse.x
    );

    scene.rotTarget.setFromSpherical(spherical);
    scene.rotation.lerp(scene.rotTarget, 0.02);
    offscreenCamera.position.copy(scene.rotation);
    offscreenCamera.lookAt(scene.focus);
    
    update(performance.now());

    doRender(renderer, scene.bg, scene.scene, offscreenCamera, fovTmp, scene.fov);
  }
  
  renderer.setViewport(0, 0, size.x, size.y);
  renderer.setScissorTest(false);
  renderer.setRenderTarget(null);
}

const renderer = new THREE.WebGLRenderer();
renderer.setPixelRatio(2);
document.body.appendChild(renderer.domElement);

const scene = new THREE.Scene();

const camera = new THREE.OrthographicCamera(-0.5, 0.5, 0.5, -0.5, 0.01, 100);
camera.position.z = 1;

const sceneObjects = createScene(scene, renderer);

const resize = () => {
  // Repeatedly resizing a large
  // render texture can anger the GPU
  cancelAnimationFrame(window.doResize);
  window.doResize = requestAnimationFrame(() => {
    const w = window.innerWidth;
    const h = window.innerHeight;
    const dpr = renderer.getPixelRatio();

    renderer.setSize(w, h);
    sceneObjects.target.setSize(w * dpr, h * dpr);
  });
};

resize();
window.addEventListener('resize', resize);
window.addEventListener('orientationchange', () => requestAnimationFrame(resize));

const mouse = new THREE.Vector3(0, 0);
window.addEventListener('pointermove', (e) => {
  mouse.set(
    (event.clientX / window.innerWidth) * 2 - 1,
    -(event.clientY / window.innerHeight) * 2 + 1
  );
  
  autoMove = false;
  clearTimeout(window.autoMoveTimeout);
  window.autoMoveTimeout = setTimeout(() => {
    autoMove = true;
  }, 5000);
});

const sceneContext = { camera, scene, sceneObjects, renderer, mouse };

const render = () => {
  updateScene(sceneContext);
  
  renderer.render(scene, camera);
  requestAnimationFrame(render);
};

requestAnimationFrame(render);